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

(** Values written into DWARF sections.
    (For attribute values, see [Dwarf_attribute_values].)
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Flag_true
  | Bool of bool
  | Int8 of Numbers.Int8.t
  | Int16 of Numbers.Int16.t
  | Int32 of Int32.t
  | Int64 of Int64.t
  | Uleb128 of Int64.t
  | Sleb128 of Int64.t
  | String of string
  | Indirect_string of string
  (* CR mshinwell: remove "Code" name. *)
  | Absolute_code_address of Targetint.t
  | Code_address_from_label of Asm_label.t
  | Code_address_from_symbol of Asm_symbol.t
  | Code_address_from_label_symbol_diff of {
      upper : Asm_label.t;
      lower : Asm_symbol.t;
      offset_upper : Targetint.t;
    }
    (** The calculation is: (upper + offset_upper) - lower. *)
  | Code_address_from_symbol_diff of {
      upper : Asm_symbol.t;
      lower : Asm_symbol.t;
    }
  | Code_address_from_symbol_plus_bytes of Asm_symbol.t * Targetint.t
  (** N.B. The basic "offset" constructors here take labels rather than
      absolute addresses---this is important so that the references are
      relocated when multiple object files are linked together (and DWARF
      information from them concatenated inside each of the various
      sections).

      - Offsets into .debug_info are of DW_FORM_ref_addr.

      - Offsets into any other section are of DW_FORM_sec_offset.
      (DWARF-4 spec p.142.)  However the widths are the same in both cases,
      corresponding to the DWARF format.
  *)
  | Offset_into_debug_info of Asm_label.t
  | Offset_into_debug_info_from_symbol of Asm_symbol.t
  | Offset_into_debug_line of Asm_label.t
  | Offset_into_debug_line_from_symbol of Asm_symbol.t
  | Offset_into_debug_loc of Asm_label.t
  (* CR mshinwell: Given that this one has to be Asm_label.t, since it comes
     from [Asm_section], then maybe everything in the DWARF parts should use
     [Asm_label.t]? *)
  | Offset_into_debug_abbrev of Asm_label.t
  | Distance_between_labels_16bit of {
      upper : Asm_label.t;
      lower : Asm_label.t;
    }
  | Distance_between_labels_32bit of {
      upper : Asm_label.t;
      lower : Asm_label.t;
    }
  | Distance_between_labels_64bit of {
      upper : Asm_label.t;
      lower : Asm_label.t;
    }

val print : Format.formatter -> t -> unit

include Dwarf_emittable.S with type t := t
