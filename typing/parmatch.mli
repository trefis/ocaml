(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Detection of partial matches and unused match cases. *)

open Asttypes
open Typedtree
open Types

module IntSet : Set.S with type elt = int

module Simple_pattern : sig
  type head =
    | Sany
    | Sarray of int
    | Sconstant of Asttypes.constant
    | Sconstruct of constructor_description
    | Slazy
    | Srecord of {
        closed : closed_flag;
        all_labels : label_description array;
        mutable fields : IntSet.t
      }
    | Stuple of int
        (* FIXME: s/has_argument : bool/arg : pattern option/ ? *)
    | Svariant of { name : label; has_argument : bool; row : row_desc ref }

  type t = {
    sp_head : head;
    sp_type : Types.type_expr;
    sp_env : Env.t;
  }
end

val pat_of_sp : Simple_pattern.t -> pattern

val omega : pattern
(** aka. "Tpat_any" or "_"  *)

val omegas : int -> pattern list

val omega_list : 'a list -> pattern list
(** [List.map (fun _ -> omega)] *)

val normalize_pat : pattern -> Simple_pattern.t

val const_compare : constant -> constant -> int
(** [const_compare c1 c2] compares the actual values represented by [c1] and
    [c2], while simply using [Pervasives.compare] would compare the
    representations.

    cf. PR#5758 *)

val le_pat : pattern -> pattern -> bool
(** [le_pat p q]  means: forall V,  V matches q implies V matches p *)

val le_pats : pattern list -> pattern list -> bool
(** [le_pats (p1 .. pm) (q1 .. qn)] means: forall i <= m, [le_pat pi qi] *)

val compat : pattern -> pattern -> bool
(** [compat p q] means: there exists V that matches both *)

val compats : pattern list -> pattern list -> bool
(** [List.for_all2 compat] *)

exception Empty

val lub : pattern -> pattern -> pattern
(** [lub p q] is a pattern that matches all values matched by [p] and [q].
    May raise [Empty], when [p] and [q] are not compatible. *)

val lubs : pattern list -> pattern list -> pattern list
(** [lubs [p1; ...; pn] [q1; ...; qk]], where [n < k], is
    [[lub p1 q1; ...; lub pk qk]].  *)

val get_mins : ('a -> 'a -> bool) -> 'a list -> 'a list

val set_args : Simple_pattern.t -> pattern list -> pattern list
(** reconstructs a pattern from a simple pattern and its arguments.
    [set_args (Stuple 2) (p1::p2::rem) -> (p1, p2)::rem]
*)

val set_args_erase_mutable : Simple_pattern.t -> pattern list -> pattern list
(** same a [set_args] but replaces mutable arguments by [_] *)

val pat_of_constr : Simple_pattern.t -> constructor_description -> pattern

val complete_constrs :
    Simple_pattern.t -> constructor_tag list -> constructor_description  list

val ppat_of_type :
    Env.t -> type_expr ->
    Parsetree.pattern *
    (string, constructor_description) Hashtbl.t *
    (string, label_description) Hashtbl.t

val pressure_variants: Env.t -> pattern list -> unit
val check_partial:
    ((string, constructor_description) Hashtbl.t ->
     (string, label_description) Hashtbl.t ->
     Parsetree.pattern -> pattern option) ->
    Location.t -> case list -> partial
val check_unused:
    (bool ->
     (string, constructor_description) Hashtbl.t ->
     (string, label_description) Hashtbl.t ->
     Parsetree.pattern -> pattern option) ->
    case list -> unit

(* Irrefutability tests *)
val irrefutable : pattern -> bool
val fluid : pattern -> bool

(* Ambiguous bindings *)
val check_ambiguous_bindings : case list -> unit
