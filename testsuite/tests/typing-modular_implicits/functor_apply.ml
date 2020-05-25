(* TEST
   * expect
*)

module Foo(P: sig end) = struct
    type t = A
end;;
[%%expect{|
module Foo : functor (P : sig end) -> sig type t = A end
|}];;

module Bar = struct end;;
[%%expect{|
module Bar : sig end
|}];;

module X: sig type t = private Foo(Bar).t end = struct
    type t = Foo(Bar).t
end ;;
[%%expect{|
module X : sig type t = private Foo(Bar).t end
|}];;
