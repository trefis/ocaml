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

module RawTypeHash = Types.TransientTypeHash

external reraise : exn -> 'a = "%reraise"

let ref_errors : (exn list ref * unit RawTypeHash.t) option ref = ref None
let ref_monitor_errors =
  (* Using a reference handle allows nested calls to be monitored.
     - the "ancestor" call should always know if there was an error
       (either directly, or in one of the nested calls)
     - a nested call should not be polluted by errors from the parent
     (or siblings) *)
  ref (ref false)

let monitor_errors () =
  let () = if !(!ref_monitor_errors)
    then ref_monitor_errors := ref false
  in
  !ref_monitor_errors

let log_or_raise exn =
  let () = !ref_monitor_errors := true in
  match !ref_errors with
  | Some (l, _) -> l := exn :: !l
  | None -> raise exn

let log_and_raise exn =
  log_or_raise exn;
  raise exn

let raise_error = log_or_raise

let catch_errors warnings caught f =
  let w = Warnings.backup () in
  let e = !ref_errors in
  let () =
    Warnings.restore warnings;
    ref_errors := Some (caught, RawTypeHash.create 3)
  in
  Misc.try_finally f ~always:(fun () ->
      ref_errors := e;
      Warnings.restore w)

let erroneous_type_check te =
  let te = Types.Transient_expr.coerce te in
  match !ref_errors with
  | Some (_, h) -> RawTypeHash.mem h te
  | _ -> false

let rec erroneous_expr_check e =
  erroneous_type_check e.Typedtree.exp_type
  || match e.Typedtree.exp_desc with
  | Typedtree.Texp_ident (p, _, _) when Ident.name (Path.head p) = "_" -> true
  | Typedtree.Texp_apply (e', _) -> erroneous_expr_check e'
  | _ -> false

let erroneous_type_register te =
  let te = Types.Transient_expr.coerce te in
  match !ref_errors with
  | Some (_, h) -> RawTypeHash.replace h te ()
  | None -> ()

let with_warning_attribute ?warning_attribute f =
  match warning_attribute with
  | None -> f ()
  | Some attr -> Builtin_attributes.warning_scope attr f

let with_saved_types ?save_part f =
  let saved_types = Cmt_format.get_saved_types () in
  Cmt_format.set_saved_types [];
  try
    let result = f () in
    begin
      match save_part with
      | None -> ()
      | Some f -> Cmt_format.set_saved_types (f result :: saved_types)
    end;
    result
  with exn ->
    let saved_types' = Cmt_format.get_saved_types () in
    Cmt_format.set_saved_types (saved_types' @ saved_types);
    reraise exn

module Saved_parts = struct
  let attribute = Location.mknoloc "ocaml.saved-parts"

  module H = Ephemeron.K1.Make(
    struct
      type t = string
      let hash = Hashtbl.hash
      let equal = String.equal
    end)

  let table = H.create 7

  let gensym =
    let counter = ref 0 in
    fun () -> incr counter; !counter

  let store parts =
    let id = string_of_int (gensym ()) in
    let key = Parsetree.Pconst_integer (id, None) in
    H.add table id parts;
    key
end

let flush_saved_types () =
  match Cmt_format.get_saved_types () with
  | [] -> []
  | parts ->
    Cmt_format.set_saved_types [];
    let open Ast_helper in
    let pconst_desc = Saved_parts.store parts in
    let pexp = Exp.constant { pconst_desc; pconst_loc = !default_loc } in
    let pstr = Str.eval pexp in
    [ Attr.mk Saved_parts.attribute (Parsetree.PStr [ pstr ]) ]

let incorrect_attribute =
  Ast_helper.Attr.mk (Location.mknoloc "ocaml.incorrect") (Parsetree.PStr [])

let recovery_attributes attrs =
  let attrs' = incorrect_attribute :: flush_saved_types () in
  match attrs with
  | [] -> attrs'
  | attrs -> attrs' @ attrs

module Error_set = Set.Make (struct
    type t = Location.error

    let compare_position (a: Lexing.position) (b: Lexing.position) =
      (* If the errors are not different, a positive number is
         returned, mainly to sort the set in order of appearance and
         to deduplicate certain errors.

         Do not take the [fname] in account. *)
      let ln = Int.compare a.pos_lnum b.pos_lnum
      and bol = Int.compare a.pos_bol b.pos_bol
      and cn = Int.compare a.pos_cnum b.pos_cnum
      in
      if Int.equal ln 0 && Int.equal bol 0 && Int.equal cn 0 then 0
      else 1


    let compare (a : t) (b : t) =
      (* If the errors are not different, a positive number is
         returned, mainly to sort the set in order of appearance and
         to deduplicate certain errors. *)
      let a_start = a.main.loc.loc_start
      and b_start = b.main.loc.loc_start in
      let a_fname = a_start.pos_fname
      and b_fname = b_start.pos_fname in
      if String.equal a_fname b_fname then
        if Int.equal (compare_position a_start b_start) 0
        then
          compare_position a.main.loc.loc_end b.main.loc.loc_end
        else 1
      else
        1
  end)
