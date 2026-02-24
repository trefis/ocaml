(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Ulysse Gérard, Thomas Refis, Frederic Bour,             *)
(*                           Xavier Van de Woestyne                       *)
(*                                                                        *)
(*   Copyright 2025 Tarides                                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Extend the given attributes with an incorrect attribute and the
    saved types after turning them into attributes *)
val recovery_attributes : Parsetree.attributes -> Parsetree.attributes

val with_saved_types :
  ?save_part:('a -> Cmt_format.binary_part) -> (unit -> 'a) -> 'a
