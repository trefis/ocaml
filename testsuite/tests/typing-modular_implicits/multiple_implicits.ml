(* TEST
   * expect
*)

(* Cf. https://github.com/ocamllabs/ocaml-modular-implicits/issues/1 *)
module type T = sig type a end
;;
[%%expect{|
module type T = sig type a end
|}];;

let f : {A: T} -> {B: T} -> A.a * B.a -> unit =
  fun {A : T} {B : T} (x : A.a * B.a) -> ()
;;
[%%expect{|
val f : {A : T} -> {B : T} -> A.a * B.a -> unit = <fun>
|}];;
