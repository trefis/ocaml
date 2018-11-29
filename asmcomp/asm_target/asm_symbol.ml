(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = string

let escape t =
  let spec = ref false in
  for i = 0 to String.length t - 1 do
    match String.unsafe_get t i with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> ()
    | _ -> spec := true;
  done;
  if not !spec then t
  else
    let b = Buffer.create (String.length t + 10) in
    String.iter
      (function
        | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') as c -> Buffer.add_char b c
        | c -> Printf.bprintf b "$%02x" (Char.code c)
      )
      t;
    Buffer.contents b

let symbol_prefix () = (* XXX *)
  match Target_system.architecture () with
  | IA32 | X86_64 ->
    begin match Target_system.system () with
    | Linux
    | Windows Cygwin
    | Windows MinGW
    | FreeBSD
    | NetBSD
    | OpenBSD
    | Generic_BSD
    | Solaris
    | BeOS
    | GNU
    | Dragonfly
    | Windows Native
    | Unknown -> "" (* checked ok. *)
    | MacOS_like -> "_" (* checked ok. *)
    end
  | ARM
  | AArch64
  | POWER
  | Z -> ""

let encode t =
  Backend_sym.to_escaped_string
    ~symbol_prefix:(symbol_prefix ())
    ~escape t

let create t = encode t

let of_external_name name = encode (Backend_sym.of_external_name name)

let encode ?reloc t =
  match reloc with
  | None -> t
  | Some reloc -> t ^ reloc

(* Detection of functions that can be duplicated between a DLL and
   the main program (PR#4690) *)

let isprefix s1 s2 =
  String.length s1 <= String.length s2
    && String.sub s2 0 (String.length s1) = s1

let is_generic_function t =
  List.exists
    (fun p -> isprefix p t)
    ["caml_apply"; "caml_curry"; "caml_send"; "caml_tuplify"]

include Identifiable.Make (struct
  type nonrec t = t
  let compare = String.compare
  let equal = String.equal
  let hash = Hashtbl.hash
  let output _ _ = Misc.fatal_error "Not yet implemented"
  let print ppf t = Format.pp_print_string ppf t
end)

module Names = struct
  let mcount = of_external_name "mcount"
  let _mcount = of_external_name "_mcount"
  let __gnu_mcount_nc = of_external_name "__gnu_mcount_nc"
  let sqrt = of_external_name "sqrt"

  let caml_young_ptr = of_external_name "caml_young_ptr"
  let caml_young_limit = of_external_name "caml_young_limit"
  let caml_exception_pointer = of_external_name "caml_exception_pointer"
  let caml_negf_mask = of_external_name "caml_negf_mask"
  let caml_absf_mask = of_external_name "caml_absf_mask"

  let caml_call_gc = of_external_name "caml_call_gc"
  let caml_c_call = of_external_name "caml_c_call"
  let caml_alloc1 = of_external_name "caml_alloc1"
  let caml_alloc2 = of_external_name "caml_alloc2"
  let caml_alloc3 = of_external_name "caml_alloc3"
  let caml_allocN = of_external_name "caml_allocN"
  let caml_ml_array_bound_error = of_external_name "caml_ml_array_bound_error"
  let caml_raise_exn = of_external_name "caml_raise_exn"

  let caml_frametable = of_external_name "caml_frametable"
  let caml_spacetime_shapes = of_external_name "caml_spacetime_shapes"
end