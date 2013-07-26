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

open Dwarf_low_dot_std
open Dwarf_std_internal

module Reg_map = struct
  include Reg.Map
  let add t ~key ~data = add key data t
  let remove t key = remove key t
  let find t reg = try Some (find reg t) with Not_found -> None
end

module Reg_set = struct
  include Reg.Set
  let iter t ~f = iter f t
  let fold t ~init ~f = fold (fun elt acc -> f acc elt) t init
  let to_list = elements
end

module One_live_range = struct
  module T = struct
    type t = {
      id : int;
      parameter_or_variable : [ `Parameter of string | `Variable ];
      reg : Reg.t;
      starting_label : Linearize.label;
      ending_label : Linearize.label;
      starts_at_beginning_of_function : bool;
    }

    let compare t t' =
      Pervasives.compare t.id t'.id
  end

  include T
  module Set = struct
    include Set.Make (T)
    let fold t ~init ~f = fold (fun elt acc -> f acc elt) t init
    let to_list = elements
  end

  let unique_id = ref 0  (* CR mshinwell: may not suffice for 32-bit *)

  let create ~parameter_or_variable ~reg ~starts_at_beginning_of_function =
    let our_id = !unique_id in
    unique_id := !unique_id + 1;
    let starting_label = Linearize.new_label () in
    let ending_label = Linearize.new_label () in
    { id = our_id;
      parameter_or_variable;
      reg;
      starting_label;
      ending_label;
      starts_at_beginning_of_function;
    }

  let code_for_starting_label t =
    Linearize.Llabel t.starting_label

  let code_for_ending_label t =
    Linearize.Llabel t.ending_label

  let parameter_or_variable t =
    t.parameter_or_variable

  let dwarf_tag t =
    match t.parameter_or_variable with
    | `Parameter _name -> Dwarf_low.Tag.formal_parameter
    | `Variable -> Dwarf_low.Tag.variable

  let reg_name t =
    match t.parameter_or_variable with
    | `Parameter name -> name
    | `Variable ->
      let name = Reg.name t.reg in
      let spilled_prefix = "spilled-" in
      if String.length name <= String.length spilled_prefix then
        name
      else if String.sub name 0 (String.length spilled_prefix) = spilled_prefix then
        String.sub name (String.length spilled_prefix)
          (String.length name - String.length spilled_prefix)
      else
        name

  let unique_name t = Printf.sprintf "%s__%d" (reg_name t) t.id

  let location_list_entry t ~start_of_function_label =
    let starting_label =  (* CR mshinwell: this is a hack *)
      if t.starts_at_beginning_of_function then
        (* CR mshinwell: it's yucky in this case that we ignore
           [t.starting_label], which will actually have been emitted.  Clean
           this up as part of the coalescing-labels work. *)
        start_of_function_label
      else
        Printf.sprintf ".L%d" t.starting_label
    in
    let ending_label =
      Printf.sprintf ".L%d" t.ending_label
    in
    let location_expression =
      match Reg.location t.reg with
      | Reg.Reg reg_number ->
        (* CR mshinwell: this needs fixing, ESPECIALLY "R".  and below.
           find out why there seems to be some problem with cloning [loc_args]
           ---we could just name them for this function if we could do that *)
        begin match reg_name t with
        | "R" | "" -> None
        | _ ->
          Some (Dwarf_low.Location_expression.in_register reg_number)
        end
      | Reg.Stack (Reg.Local stack_slot_index) ->
        Some (Dwarf_low.Location_expression.at_offset_from_stack_pointer
            ~offset_in_bytes:(stack_slot_index * 8))
      | Reg.Stack (Reg.Incoming _) -> None  (* CR mshinwell: don't know *)
      | Reg.Stack (Reg.Outgoing _) -> None
      | Reg.Unknown -> None
    in
    match location_expression with
    | None -> None
    | Some location_expression ->
      let location_list_entry =
        Dwarf_low.Location_list_entry.create_location_list_entry
          ~start_of_code_label:start_of_function_label
          ~first_address_when_in_scope:starting_label
          ~first_address_when_not_in_scope:ending_label
          ~location_expression
      in
      Some location_list_entry
end

(* CR mshinwell: this many/one live ranges thing isn't great; consider
   restructuring *)

module Many_live_ranges = struct
  type t = {
    live_ranges : One_live_range.t list
  }

  let create live_ranges =
    match live_ranges with
    | [] -> { live_ranges = []; }
    | live_range::_remainder ->
(* doesn't work
      let validate live_range' =
        (* CR mshinwell: note that this doesn't actually make sure the live
           ranges are for the same variable. *)
        Pervasives.(=) (One_live_range.parameter_or_variable live_range)
          (One_live_range.parameter_or_variable live_range')
      in
      if not (List.for_all validate live_ranges) then
        Misc.fatal_error "Many_live_ranges.create: validation failed";
*)
      { live_ranges; }

  let dwarf_tag t =
    (* CR mshinwell: needs sorting out too *)
    match List.dedup (List.map t.live_ranges ~f:One_live_range.dwarf_tag) with
    | [tag] -> tag
    | [] -> Dwarf_low.Tag.variable (* doesn't get used *)
    | _tags -> Dwarf_low.Tag.formal_parameter

  let name t =
    (* CR mshinwell: the name handling needs thought.  Maybe we should
       attach properly-stamped idents to Regs?  This must be required to
       fix problems when names are shadowed. *)
    let names = List.map t.live_ranges ~f:One_live_range.reg_name in
    let without_dummies =
      List.filter names ~f:(function "R" | "" -> false | _ -> true)
    in
    match List.dedup without_dummies with
    | [name] -> name
    | [] -> "<anon>"
    | multiple -> String.concat "/" multiple

  let dwarf_attribute_values t ~builtin_ocaml_type_label_value
        ~debug_loc_table ~start_of_function_label =
    let base_address_selection_entry =
      Dwarf_low.Location_list_entry.create_base_address_selection_entry
        ~base_address_label:start_of_function_label
    in
    let location_list_entries =
      List.filter_map t.live_ranges
        ~f:(One_live_range.location_list_entry ~start_of_function_label)
    in
    match location_list_entries with
    | [] -> [], debug_loc_table
    | location_list_entries ->
      let location_list =
        Dwarf_low.Location_list.create
          (base_address_selection_entry :: location_list_entries)
      in
      let debug_loc_table, loclistptr_attribute_value =
        Dwarf_low.Debug_loc_table.insert debug_loc_table
          ~location_list
      in
      let type_label_name = builtin_ocaml_type_label_value in
      let attribute_values =
        [Dwarf_low.Attribute_value.create_name ~source_file_path:(name t);
         loclistptr_attribute_value;
         Dwarf_low.Attribute_value.create_type ~label_name:type_label_name;
        ]
      in
      attribute_values, debug_loc_table

  let to_dwarf t ~debug_loc_table ~builtin_ocaml_type_label_value
        ~start_of_function_label =
    let tag = dwarf_tag t in
    let attribute_values, debug_loc_table =
      dwarf_attribute_values t
        ~builtin_ocaml_type_label_value
        ~debug_loc_table
        ~start_of_function_label
    in
    tag, attribute_values, debug_loc_table
end

(* CR mshinwell: thought: find out how C++ compilers emit DWARF for local
   variables that are defined not at the start of a block. *)

let rec process_instruction ~insn ~first_insn ~prev_insn ~current_live_regs
      ~current_live_ranges ~previous_live_ranges ~fundecl =
  let regs_live_across_this_insn =
    (* CR mshinwell: need to precisely understand the semantics of "arg" *)
    Array.fold_left (fun regs reg -> Reg_set.add reg regs)
      insn.Linearize.live
      insn.Linearize.arg
  in
  let must_start_live_ranges_for =
    (* Regs whose live ranges will start immediately before this insn. *)
    Reg_set.diff regs_live_across_this_insn current_live_regs
  in
  let must_finish_live_ranges_for =
    (* Regs whose live ranges will stop immediately before this insn. *)
    Reg_set.diff current_live_regs regs_live_across_this_insn
  in
  let current_live_ranges, labels_to_insert_before_insn =
    Reg_set.fold must_start_live_ranges_for
      ~init:(current_live_ranges, [])
      ~f:(fun (current_live_ranges, labels_to_insert_before_insn) reg ->
            let parameter_or_variable =
              match Reg.is_parameter reg with
              | Some parameter_index ->
                let parameter_name =
                  (* CR mshinwell: slow and unsafe, but will do for now *)
                  try
                  let ident =
                    fst ((Array.of_list fundecl.Linearize.fun_args_and_locations).
                      (parameter_index))
                  in
                  Ident.name ident
                  with _exn -> Printf.sprintf "<parameter %d>" parameter_index
                in
                `Parameter parameter_name
              | None ->
                `Variable
            in
            let live_range =
              One_live_range.create ~parameter_or_variable ~reg
                ~starts_at_beginning_of_function:(prev_insn = None)
            in
            let current_live_ranges =
              Reg_map.add current_live_ranges ~key:reg ~data:live_range
            in
            (* CR mshinwell: How about just ONE label for all of these?
               Likewise below. *)
            let labels_to_insert_before_insn =
              (One_live_range.code_for_starting_label live_range)
                :: labels_to_insert_before_insn
            in
            current_live_ranges, labels_to_insert_before_insn)
  in
  let current_live_ranges, previous_live_ranges, labels_to_insert_before_insn =
    Reg_set.fold must_finish_live_ranges_for
      ~init:(current_live_ranges, previous_live_ranges,
             labels_to_insert_before_insn)
      ~f:(fun (current_live_ranges, previous_live_ranges,
               labels_to_insert_before_insn) reg ->
            match Reg_map.find current_live_ranges reg with
            | None -> assert false
            | Some live_range ->
              let current_live_ranges =
                Reg_map.remove current_live_ranges reg
              in
              let previous_live_ranges =
                live_range :: previous_live_ranges
              in
              let labels_to_insert_before_insn =
                (One_live_range.code_for_ending_label live_range)
                  :: labels_to_insert_before_insn
              in
              current_live_ranges, previous_live_ranges,
                labels_to_insert_before_insn)
  in
  if List.length labels_to_insert_before_insn > 0 then begin
    (* Inserting the code to emit the live range labels is complicated by the
       structure of values of type [Linearize.instruction]. *)
    let first_insn', last_insn' =
      List.fold_left labels_to_insert_before_insn
        ~init:(None, None)
        ~f:(fun (first_insn, prev_insn) desc ->
              let insn =
                { Linearize.
                  desc;
                  next = insn;  (* dummy value, will be fixed below *)
                  arg = [| |];
                  res = [| |];
                  dbg = insn.Linearize.dbg;
                  live = insn.Linearize.live;
                }
              in
              begin match prev_insn with
              | None -> ()
              | Some prev_insn -> prev_insn.Linearize.next <- insn
              end;
              let first_insn =
                match first_insn with
                | None -> Some insn
                | Some first_insn -> Some first_insn
              in
              first_insn, Some insn)
    in
    let first_insn' =
      match first_insn' with
      | None -> assert false
      | Some first_insn' -> first_insn'
    in
    let last_insn' =
      match last_insn' with
      | None -> assert false
      | Some last_insn' -> last_insn'
    in
    (* [first_insn'] .. [last_insn'] is now a correctly-linked sequence of
       instructions with the exception that [last_insn'] has an invalid
       [next] pointer.  We fix that first and then splice the sequence into
       the existing list of instructions. *)
    last_insn'.Linearize.next <- insn;
    match prev_insn with
    | None ->
      (* We need:  [first_insn'] .. [last_insn'] -> [insn]
         and we need to return [first_insn'] as the new first instruction. *)
      first_insn := first_insn'
    | Some prev_insn ->
      (* We need:  [prev_insn] -> [first_insn'] .. [last_insn'] -> [insn]. *)
      assert (prev_insn.Linearize.next == insn);
      prev_insn.Linearize.next <- first_insn'
  end;
  match insn.Linearize.desc with
  | Linearize.Lend -> !first_insn, previous_live_ranges
  | Linearize.Lop _ | Linearize.Lreloadretaddr | Linearize.Lreturn
  | Linearize.Llabel _ | Linearize.Lbranch _ | Linearize.Lcondbranch _
  | Linearize.Lcondbranch3 _ | Linearize.Lswitch _ | Linearize.Lsetuptrap _
  | Linearize.Lpushtrap | Linearize.Lpoptrap | Linearize.Lraise ->
    process_instruction ~insn:insn.Linearize.next
      ~first_insn
      ~prev_insn:(Some insn)
      ~current_live_regs:regs_live_across_this_insn
      ~current_live_ranges
      ~previous_live_ranges
      ~fundecl

let process_fundecl fundecl =
  let first_insn, live_ranges =
    process_instruction ~insn:fundecl.Linearize.fun_body
      ~first_insn:(ref fundecl.Linearize.fun_body)
      ~prev_insn:None
      ~current_live_regs:Reg_set.empty
      ~current_live_ranges:Reg_map.empty
      ~previous_live_ranges:[]
      ~fundecl
  in
  let name_map =
    List.fold live_ranges
      ~init:String.Map.empty
      ~f:(fun name_map live_range ->
            let name = One_live_range.reg_name live_range in
            match String.Map.find name_map name with
            | None -> String.Map.add name_map ~key:name ~data:[live_range]
            | Some live_ranges ->
              let data = live_range::live_ranges in
              String.Map.add (* replace *) name_map ~key:name ~data)
  in
  let live_ranges =
    List.map (List.map (String.Map.to_alist name_map) ~f:snd)
      ~f:Many_live_ranges.create
  in
  live_ranges, { fundecl with Linearize. fun_body = first_insn; }
