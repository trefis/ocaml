(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013, Jane Street Holding                                *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

(* CR-soon mshinwell: fix uses of [open] *)
open Dwarf_low_dot_std
open Dwarf_low
open Std_internal

type t = {
  emitter : Emitter.t;
  source_file_path : string option;
  start_of_code_label : string;
  end_of_code_label : string;
  mutable externally_visible_functions : string list;
  mutable function_tags :
    (int * string * Tag.t * Dwarf_low.Attribute_value.t list) list;
  mutable debug_loc_table : Debug_loc_table.t;
}

let create ~emitter ~source_file_path ~start_of_code_label ~end_of_code_label =
  { emitter;
    source_file_path;
    start_of_code_label;
    end_of_code_label;
    externally_visible_functions = [];
    function_tags = [];
    debug_loc_table = Debug_loc_table.create ();
  }

module Function = struct
  type t = string  (* function name, ahem *)
end

let start_function t ~function_name =
  let starting_label = sprintf "Llr_begin_%s" function_name in
  let ending_label = sprintf "Llr_end_%s" function_name in
  Emitter.emit_label_declaration t.emitter starting_label;
  let debug_loc_table = t.debug_loc_table in
  (* CR-soon mshinwell: This is where the function argument code goes. *)
  let argument_tags = [] in
  let subprogram_tag =
    let tag =
      if List.length argument_tags > 0 then
        Tag.subprogram
      else
        Tag.subprogram_with_no_children
    in
    let module AV = Attribute_value in
    1, function_name, tag, [
      AV.create_name ~source_file_path:function_name;
      AV.create_external ~is_visible_externally:true;
      AV.create_low_pc ~address_label:starting_label;
      AV.create_high_pc ~address_label:ending_label;
    ]
  in
  let this_function's_tags = subprogram_tag::(List.rev argument_tags) in
  t.externally_visible_functions <-
    function_name::t.externally_visible_functions;
  t.debug_loc_table <- debug_loc_table;
  t.function_tags <- t.function_tags @ this_function's_tags;
  function_name

let end_function t function_name =
  Emitter.emit_label_declaration t.emitter (sprintf "Llr_end_%s" function_name)

let with_emitter emitter fs =
  List.iter (fun f -> f emitter) fs

let emit_debugging_info_prologue t =
  let module SN = Section_names in
  with_emitter t.emitter [
    Emitter.emit_section_declaration ~section_name:SN.debug_abbrev;
    Emitter.emit_label_declaration ~label_name:"Ldebug_abbrev0";
    Emitter.emit_section_declaration ~section_name:SN.debug_line;
    Emitter.emit_label_declaration ~label_name:"Ldebug_line0";
    Emitter.emit_section_declaration ~section_name:SN.debug_loc;
    Emitter.emit_label_declaration ~label_name:"Ldebug_loc0";
  ]

let emit_debugging_info_epilogue t =
  let emitter = t.emitter in
  let producer_name = sprintf "ocamlopt %s" Sys.ocaml_version in
  let compile_unit_attribute_values =
    let module AV = Attribute_value in
    let common = [
      AV.create_producer ~producer_name;
      AV.create_low_pc ~address_label:t.start_of_code_label;
      AV.create_high_pc ~address_label:t.end_of_code_label;
      AV.create_stmt_list ~section_offset_label:"Ldebug_line0";
      AV.create_comp_dir ~directory:(Sys.getcwd ());
    ]
    in
    match t.source_file_path with
    | None -> common
    | Some source_file_path -> (AV.create_name ~source_file_path)::common
  in
  let tags_with_attribute_values = [
    0, "compile_unit",
      Tag.compile_unit, compile_unit_attribute_values;
    (* CR-soon mshinwell: resurrect the type tags here *)
  ] (* @ (build_ocaml_type_tags ()) *) @ t.function_tags
  in
  let debug_info = Debug_info_section.create ~tags_with_attribute_values in
  let debug_abbrev = Debug_info_section.to_abbreviations_table debug_info in
  let pubnames_table =
    Pubnames_table.create
      ~externally_visible_functions:t.externally_visible_functions
      ~debug_info
  in
  let aranges_table =
    Aranges_table.create ~start_of_code_label:t.start_of_code_label
      ~end_of_code_label:t.end_of_code_label
  in
  let module SN = Section_names in
  (* CR-someday mshinwell: consider using [with_emitter] *)
  Emitter.emit_section_declaration emitter ~section_name:SN.debug_info;
  Emitter.emit_label_declaration emitter ~label_name:"Ldebug_info0";
  Debug_info_section.emit debug_info ~emitter;
  Emitter.emit_switch_to_section emitter ~section_name:SN.debug_abbrev;
  Abbreviations_table.emit debug_abbrev ~emitter;
  Emitter.emit_section_declaration emitter ~section_name:SN.debug_pubnames;
  Pubnames_table.emit pubnames_table ~emitter;
  Emitter.emit_section_declaration emitter ~section_name:SN.debug_aranges;
  Aranges_table.emit aranges_table ~emitter;
  Emitter.emit_switch_to_section emitter ~section_name:SN.debug_loc;
  Debug_loc_table.emit t.debug_loc_table ~emitter
