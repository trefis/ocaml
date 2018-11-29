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

(** Generation of DWARF debugging information for OCaml compilation units. *)

type t

(** Create a value of type [t], which holds all state necessary to emit
    DWARF debugging information for a single compilation unit. *)
val create : prefix_name:string -> t

(** For dealing with [Let_symbol] bindings. *)
val dwarf_for_toplevel_constants
   : t
  -> Clambda.preallocated_constant list
  -> unit

(** For dealing with [Initialize_symbol] bindings. *)
val dwarf_for_toplevel_inconstants
   : t
  -> Clambda.preallocated_block list
  -> unit

(** Prepare a function definition for DWARF emission, emit the function using
    the given emitter, and then generate corresponding DWARF. *)
val dwarf_for_fundecl_and_emit
   : t
  -> emit:(Linearize.fundecl -> end_of_function_label:Linearize.label -> unit)
  -> end_of_function_label:Linearize.label
  -> Linearize.fundecl
  -> unit

(** Write the DWARF information to the assembly file.  This should only be
    called once all (in)constants and function declarations have been passed
    to the above functions. *)
val emit : t -> unit