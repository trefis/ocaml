(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";  
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let g f = f ~x:0 ~y:0; f ~y:0 ~x:0
let h (f: x:int -> y:int -> unit) = f ~x:0 ~y:0; f ~y:0 ~x:0

let u f x y =
  match f ~x ~y, f ~y ~x with
  | 10, x -> 20, x
  | x, y -> x + 10, y

let t : int = ""
