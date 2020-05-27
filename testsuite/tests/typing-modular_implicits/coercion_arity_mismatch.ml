(* TEST
   * expect
*)

(* Cf. https://github.com/ocamllabs/ocaml-modular-implicits/issues/10 *)

module type S = sig type t end;;
module type T = sig type _ t end;;
[%%expect{|
module type S = sig type t end
module type T = sig type _ t end
|}];;

(* FIXME: not exactly the error message we want. *)
let f (x : {X : S} -> X.t) () = (x :> {X : T} -> unit X.t);;
[%%expect{|
Line 1, characters 32-58:
1 | let f (x : {X : S} -> X.t) () = (x :> {X : T} -> unit X.t);;
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type {X/1: S} -> X/1.t is not a subtype of {X/2: T} -> unit X/2.t
|}];;
