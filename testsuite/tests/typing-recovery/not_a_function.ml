(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

(* FIXME: recovery isn't happening as it should be here.

   From what little investigating I've done, it seems that:
   - [split_function_ty] (well, filter_arrow really) fails on [(fun x -> x)]
     when the expected type is bool
   - the recovery logs the error, pretends to split by:
      + returning the expected type as return type
      + synthesizing a parameter at type [newvar2 level]
   - then a few lines below [tpoly_get_poly] asserts false

   Somehow the assertion failure is caught somewhere by a [try .. with _ -> ..]
   and we "recover", but the value binding is dropped..

   I tried fixing [split_function_ty] so it would synthesize a "correct"
   parameter type (hitting internal assert false is bad!), but this degrades a
   lot of error messages when the recovery is on, so I refrained for now.

   I think the best course of action at this point is to:
     1. unhide the assertion failure (probably by getting rid of the catch all
        patterns mentioned above, of which there are only 3 in typecore)
     2. avoid that assert false by either using log_and_raise instead of
        log_or_raise or by giving a "correct" type to the param (that is
        [newty2 ~level (Tpoly (newvar2 level, []))]) and then understand how the
        error is propagated to try to keep the good error message
*)

let x =
  let a : string = 10 in
  let () = if (fun x -> x) then () in
  a ^ "foo"

module R = struct
  let x =
    let a : string = 10 in
    let () = if (fun x -> x) then () in
    a ^ "foo"
end

let () = if (fun x -> x) then () else ()

let f () =
  String.capitalize_ascii (x ^ R.x)

let t : bool = 0
