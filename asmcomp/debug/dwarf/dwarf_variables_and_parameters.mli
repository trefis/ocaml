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

(** Handling of DWARF descriptions of variables and function parameters. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val dwarf
   : Dwarf_state.t
  -> Linearize.fundecl
  -> function_proto_die:Proto_die.t
  -> scope_proto_dies:Proto_die.t Numbers.Int.Map.t
  -> Available_ranges_all_vars.t
  -> unit
