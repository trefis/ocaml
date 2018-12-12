(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module A = Dwarf_attributes.Attribute
module AS = Dwarf_attributes.Attribute_specification
module AV = Dwarf_attribute_values.Attribute_value
module F = Dwarf_attributes.Form
module V = Dwarf_attribute_values.Value

module Uint64 = Numbers.Uint64

let needs_dwarf_five () =
  match !Clflags.dwarf_version with
  | Four -> Misc.fatal_error "Attribute not supported for DWARF-4"
  | Five -> ()

let create_low_pc ~address_label =
  let spec = AS.create A.Low_pc F.Addr in
  AV.create spec (V.code_address_from_label
    ~comment:"low PC value" address_label)

let create_high_pc ~address_label =
  let spec = AS.create A.High_pc F.Addr in
  AV.create spec (V.code_address_from_label
    ~comment:"high PC value" address_label)

let create_low_pc_from_symbol ~symbol =
  let spec = AS.create A.Low_pc F.Addr in
  AV.create spec (V.code_address_from_symbol
    ~comment:"low PC value" symbol)

let create_high_pc_from_symbol ~symbol =
  let spec = AS.create A.High_pc F.Addr in
  AV.create spec (V.code_address_from_symbol
    ~comment:"high PC value" symbol)

let create_producer ~producer_name =
  let spec = AS.create A.Producer F.Strp in
  AV.create spec (V.indirect_string ~comment:"producer name" producer_name)

let create_name name =
  let spec = AS.create A.Name F.Strp in
  AV.create spec (V.indirect_string ~comment:"name" name)

let create_comp_dir ~directory =
  let spec = AS.create A.Comp_dir F.Strp in
  AV.create spec (V.indirect_string ~comment:"compilation directory" directory)

let create_stmt_list ~debug_line_label =
  let spec = AS.create A.Stmt_list F.Sec_offset_lineptr in
  (* DWARF-4 standard section 3.1.1.4. *)
  AV.create spec (V.offset_into_debug_line debug_line_label)

let create_external ~is_visible_externally =
  if is_visible_externally then
    let spec = AS.create A.External F.Flag_present in
    AV.create spec (V.flag_true ~comment:"visible externally" ())
  else
    let spec = AS.create A.External F.Flag in
    AV.create spec (V.bool ~comment:"not visible externally" false)

let create_call_file file =
  needs_dwarf_five ();
  let spec = AS.create A.Call_file F.Udata in
  let file = Uint64.of_int_exn file in
  AV.create spec (V.uleb128 ~comment:"file number" file)

let create_call_line line =
  needs_dwarf_five ();
  let spec = AS.create A.Call_line F.Udata in
  let line = Uint64.of_int_exn line in
  AV.create spec (V.uleb128 ~comment:"line number" line)

let create_call_column column =
  needs_dwarf_five ();
  let spec = AS.create A.Call_column F.Udata in
  let column = Uint64.of_int_exn column in
  AV.create spec (V.uleb128 ~comment:"column number" column)

let create_call_pc label =
  let spec = AS.create A.Call_pc F.Addr in
  needs_dwarf_five ();
  AV.create spec (V.code_address_from_label ~comment:"PC of call site" label)

let create_call_return_pc label =
  needs_dwarf_five ();
  let spec = AS.create A.Call_return_pc F.Addr in
  AV.create spec (V.code_address_from_label
    ~comment:"PC immediately after call site" label)

let create_call_tail_call ~is_tail =
  if is_tail then
    let spec =
      match !Clflags.dwarf_version with
      | Four -> AS.create (A.Dwarf_4 GNU_tail_call) F.Flag_present
      | Five -> AS.create A.Call_tail_call F.Flag_present
    in
    AV.create spec (V.flag_true ~comment:"is a tail call" ())
  else
    let spec =
      match !Clflags.dwarf_version with
      | Four -> AS.create (A.Dwarf_4 GNU_tail_call) F.Flag
      | Five -> AS.create A.Call_tail_call F.Flag
    in
    AV.create spec (V.bool ~comment:"is a non-tail call" false)

let create_call_all_calls () =
  let spec =
    match !Clflags.dwarf_version with
    | Four -> AS.create (A.Dwarf_4 GNU_all_call_sites) F.Flag_present
    | Five -> AS.create A.Call_all_calls F.Flag_present
  in
  AV.create spec (V.flag_true ~comment:"DW_AT_call_all_calls is set" ())

let create_call_target loc_desc =
  let spec =
    match !Clflags.dwarf_version with
    | Four -> AS.create (A.Dwarf_4 GNU_call_site_target) F.Exprloc
    | Five -> AS.create A.Call_target F.Exprloc
  in
  AV.create spec (V.single_location_description loc_desc)

let create_call_target_clobbered loc_desc =
  let spec =
    match !Clflags.dwarf_version with
    | Four -> AS.create (A.Dwarf_4 GNU_call_site_target_clobbered) F.Exprloc
    | Five -> AS.create A.Call_target_clobbered F.Exprloc
  in
  AV.create spec (V.single_location_description loc_desc)

let create_location index =
  needs_dwarf_five ();
  let location_list_label = Location_list_table.Index.to_label index in
  let location_list_index = Location_list_table.Index.to_uint64 index in
  if not !Clflags.dwarf_location_and_range_table_offsets then
    let spec = AS.create A.Location F.Sec_offset_loclist in
    AV.create spec (V.offset_into_debug_loclists location_list_label)
  else
    let spec = AS.create A.Location F.Loclistx in
    AV.create spec (V.loclistx ~index:location_list_index)

let create_ranges index =
  needs_dwarf_five ();
  let range_list_label = Range_list_table.Index.to_label index in
  let range_list_index = Range_list_table.Index.to_uint64 index in
  if not !Clflags.dwarf_location_and_range_table_offsets then
    let spec = AS.create A.Ranges F.Sec_offset_rnglist in
    AV.create spec (V.offset_into_debug_rnglists range_list_label)
  else
    let spec = AS.create A.Ranges F.Rnglistx in
    AV.create spec (V.rnglistx ~index:range_list_index)

let create_single_location_description loc_desc =
  let spec = AS.create A.Location F.Exprloc in
  AV.create spec (V.single_location_description loc_desc)

let create_composite_location_description loc_desc =
  let spec = AS.create A.Location F.Exprloc in
  AV.create spec (V.composite_location_description loc_desc)

let create_single_call_data_location_description loc_desc =
  needs_dwarf_five ();
  let spec = AS.create A.Call_data_location F.Exprloc in
  AV.create spec (V.single_location_description loc_desc)

let create_single_call_data_value_location_description loc_desc =
  let spec =
    match !Clflags.dwarf_version with
    | Four -> AS.create (A.Dwarf_4 GNU_call_site_data_value) F.Exprloc
    | Five -> AS.create A.Call_data_value F.Exprloc
  in
  AV.create spec (V.single_location_description loc_desc)

let create_encoding ~encoding =
  let spec = AS.create A.Encoding F.Data1 in
  AV.create spec (V.encoding_attribute encoding)

let reference_proto_die attribute proto_die =
  let spec = AS.create attribute F.Ref_addr in
  let label = Proto_die.reference proto_die in
  AV.create spec (V.offset_into_debug_info ~comment:"ref. to DIE" label)

let create_type ~proto_die = reference_proto_die A.Type proto_die
let create_sibling ~proto_die = reference_proto_die A.Sibling proto_die
let create_import ~proto_die = reference_proto_die A.Import proto_die

let create_type_from_reference ~proto_die_reference:label =
  let spec = AS.create A.Type F.Ref_addr in
  AV.create spec (V.offset_into_debug_info
    ~comment:"reference to type DIE" label)

(* CR-soon mshinwell: remove "_exn" prefix. *)
let create_byte_size_exn ~byte_size =
  let spec = AS.create A.Byte_size F.Data8 in
  AV.create spec (V.int64 ~comment:"byte size" (Int64.of_int byte_size))

let create_bit_size bit_size =
  let spec = AS.create A.Bit_size F.Data8 in
  AV.create spec (V.int64 ~comment:"bit size" bit_size)

let create_data_member_location ~byte_offset =
  let spec = AS.create A.Data_member_location F.Data8 in
  AV.create spec (V.int64 ~comment:"data member location" byte_offset)

let create_linkage_name ~linkage_name =
  let spec = AS.create A.Linkage_name F.Strp in
  AV.create spec (V.indirect_string ~comment:"linkage name"
    (Linkage_name.to_string linkage_name))

let create_const_value_from_symbol ~symbol =
  match Targetint.size with
  | 32 ->
    let spec = AS.create A.Const_value F.Data4 in
    AV.create spec (V.symbol_32 symbol)
  | 64 ->
    let spec = AS.create A.Const_value F.Data8 in
    AV.create spec (V.symbol_64 symbol)
  | size -> Misc.fatal_errorf "Unknown Targetint.size %d" size

let create_addr_base label =
  let spec = AS.create A.Addr_base F.Sec_offset_addrptr in
  AV.create spec (V.offset_into_debug_addr label)

let create_loclists_base label =
  let spec = AS.create A.Loclists_base F.Sec_offset_loclistsptr in
  AV.create spec (V.offset_into_debug_loclists label)

let create_rnglists_base label =
  let spec = AS.create A.Rnglists_base F.Sec_offset_rnglistsptr in
  AV.create spec (V.offset_into_debug_rnglists label)
