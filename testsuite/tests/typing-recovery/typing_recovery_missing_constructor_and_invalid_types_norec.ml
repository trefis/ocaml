(* TEST
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";  
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type t = A | B
         
let f (x : t) =
  match x with
  | A -> ()
  | B -> 3
  | C -> 'a'
