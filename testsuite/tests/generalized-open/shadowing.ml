(* TEST
* setup-ocamlc.byte-build-env
** ocamlc.byte
ocamlc_byte_exit_status = "0"
** check-ocamlc.byte-output
*)

type t = A

open struct
  type t = B
end;;
