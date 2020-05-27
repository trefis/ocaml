(* TEST
   * expect
*)

module type T = sig type t val x : t end;;
[%%expect{|
module type T = sig type t val x : t end
|}];;

(* BAD *)
let f () =
  let x = ref [] in
  let g {M : T} () = x := [M.x] in
    ();;
[%%expect{|
Line 3, characters 27-30:
3 |   let g {M : T} () = x := [M.x] in
                               ^^^
Error: This expression has type M.t but an expression was expected of type 'a
       The type constructor M.t would escape its scope
|}];;

(* FIXME! (occur_univar_and_implicit?) *)
(* BAD *)
let f (x : 'a) {M : T} =
  (x : M.t);
  ();;
[%%expect{|
Line 2, characters 2-11:
2 |   (x : M.t);
      ^^^^^^^^^
Warning 10: this expression should have type unit.
val f : M/1.t -> {M/2: T} -> unit = <fun>
|}];;

(* OK *)
let f {M : T} (x : M.t) y =
  (y : M.t);
  ();;
[%%expect{|
Line 2, characters 2-11:
2 |   (y : M.t);
      ^^^^^^^^^
Warning 10: this expression should have type unit.
val f : {M: T} -> M.t -> M.t -> unit = <fun>
|}];;

(* OK *)
let rec f {M : T} (x : M.t) = ();;
[%%expect{|
val f : {M: T} -> M.t -> unit = <fun>
|}];;

(* OK *)
let rec f {M : T} (x : M.t) y =
  (y : M.t);
  ();;
[%%expect{|
Line 2, characters 2-11:
2 |   (y : M.t);
      ^^^^^^^^^
Warning 10: this expression should have type unit.
val f : {M: T} -> M.t -> M.t -> unit = <fun>
|}];;

(* FIXME: that error doesn't look right. *)
(* BAD *)
let f : {M : T} -> 'a -> unit =
  fun {M : T} (x : M.t) -> ();;
[%%expect{|
Line 2, characters 2-29:
2 |   fun {M : T} (x : M.t) -> ();;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type {M/1: T} -> M/1.t -> unit
       but an expression was expected of type {M/3: T} -> M/2.t -> unit
       Type M/1.t is not compatible with type M/2.t
|}];;

(* OK *)
let f (g : {M : T} -> M.t -> unit) () = ();;
[%%expect{|
val f : ({M: T} -> M.t -> unit) -> unit -> unit = <fun>
|}];;

(* OK *)
let f {M : T} {N : T} = N.x;;
[%%expect{|
val f : {M: T} -> {N: T} -> N.t = <fun>
|}];;
