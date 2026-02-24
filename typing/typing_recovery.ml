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

let uncatch_errors f =
  let e = !ref_errors in
  ref_errors := None;
  Misc.try_finally f ~always:(fun () -> ref_errors := e)


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
