(* TEST
   * expect
*)

type 'a r = <w: int -> int; .. > as 'a;;
[%%expect{|
type 'a r = 'a constraint 'a = < w : int -> int; .. >
|}];;

class type virtual ct = object('self)
  constraint 'self = 'not_self r
end;;
[%%expect{|
Lines 1-3, characters 24-3:
1 | ........................object('self)
2 |   constraint 'self = 'not_self r
3 | end..
Warning 17 [undeclared-virtual-method]: the virtual method w is not declared.
class type virtual ct = object method virtual w : int -> int end
|}];;
