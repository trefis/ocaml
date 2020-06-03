(* TEST
   * expect
*)

type z = Z
type 'n s = S of 'n;;
[%%expect{|
type z = Z
type 'n s = S of 'n
|}];;

module type N = sig
  type n
  val n : n
end;;
[%%expect{|
module type N = sig type n val n : n end
|}];;

implicit module Z : N with type n = z = struct
  type n = z
  let n = Z
end;;
[%%expect{|
module Z : sig type n = z val n : n end
|}];;

implicit module S {N : N} : N with type n = N.n s = struct
  type n = N.n s
  let n = S N.n
end;;
[%%expect{|
module S : functor (N : N) -> sig type n = N.n s val n : n end
|}];;

module type ADD = sig
  type a and b and c
  val a : a
  val b : b
  val c : c
end;;
[%%expect{|
module type ADD = sig type a and b and c val a : a val b : b val c : c end
|}];;

let add {Add : ADD} (a: Add.a) (b : Add.b) : Add.c = Add.c;;
[%%expect{|
val add : {Add: ADD} -> Add.a -> Add.b -> Add.c = <fun>
|}];;

implicit module AddZ {B : N} : ADD with type a = z
                                    and type b = B.n
                                    and type c = B.n =
struct
  type a = z and b = B.n and c = B.n
  let  a = Z and b = B.n and c = B.n
end;;
[%%expect{|
module AddZ :
  functor (B : N) ->
    sig type a = z and b = B.n and c = B.n val a : a val b : b val c : c end
|}];;

implicit module AddS {A: N} {B: N} {Add : ADD with type a = A.n and
type b = B.n}
       : ADD with type a = A.n s
              and type b = B.n
              and type c = Add.c s =
struct
  type a = A.n s and b = B.n and c = Add.c s
  let  a = S A.n and b = B.n and c = S Add.c
end;;
[%%expect{|
module AddS :
  functor (A : N) (B : N)
    (Add : sig
             type a = A.n
             and b = B.n
             and c
             val a : a
             val b : b
             val c : c
           end)
    ->
    sig
      type a = A.n s
      and b = B.n
      and c = Add.c s
      val a : a
      val b : b
      val c : c
    end
|}];;

(* Stress Implicitsearch.translpath
   Resolved code: add (implicit AddS(Z)(Z)(AddZ(Z))) (S Z) Z *)
add (S Z) Z;;
[%%expect{|
Line 1, characters 0-3:
1 | add (S Z) Z;;
    ^^^
Error: No instance found for implicit Add where:
  - type Add.c = imp#
  - type Add.b = z
  - type Add.a = z s
|}];;
