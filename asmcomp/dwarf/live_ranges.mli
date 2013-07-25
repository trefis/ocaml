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

(* Use liveness information in a linearized function declaration to
   produce DWARF attributes describing the live ranges of named
   variables.  A new linearized function declaration is returned that
   contains labels, referenced from the DWARF attributes, identifying
   the live ranges.
*)

open Dwarf_low_dot_std

module One_live_range : sig
  type t

  val unique_name : t -> string

  val to_dwarf : t
    -> debug_loc_table:Dwarf_low.Debug_loc_table.t
    -> builtin_ocaml_type_label_value:string
    -> Dwarf_low.Tag.t * Dwarf_low.Attribute_value.t list
         * Dwarf_low.Debug_loc_table.t
end

(* [process_fundecl fundecl] may modify [fundecl] in-place by inserting label
   declarations. *)
val process_fundecl : Linearize.fundecl
  -> One_live_range.t list * Linearize.fundecl
