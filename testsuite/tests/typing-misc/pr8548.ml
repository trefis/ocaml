(* TEST
   * expect *)

module type Endpoint_intf = sig
  type t
end
;;
[%%expect{|
module type Endpoint_intf = sig type t end
|}]

module type S = sig
  module Endpoint : Endpoint_intf

  type finite = [ `Before of Endpoint.t ]
  type infinite = [ `Until_infinity ]

  type +'a range = private { until : 'a } constraint 'a = [< finite | infinite ]

  val until : 'a range -> 'a
end
;;
[%%expect{|
module type S =
  sig
    module Endpoint : Endpoint_intf
    type finite = [ `Before of Endpoint.t ]
    type infinite = [ `Until_infinity ]
    type +'a range = private { until : 'a; }
      constraint 'a = [< `Before of Endpoint.t | `Until_infinity ]
    val until :
      ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
  end
|}]

module type Ranged = sig
  module Endpoint : Endpoint_intf
  module Range : S with type Endpoint.t = Endpoint.t
end
;;
[%%expect{|
module type Ranged =
  sig
    module Endpoint : Endpoint_intf
    module Range :
      sig
        module Endpoint : sig type t = Endpoint.t end
        type finite = [ `Before of Endpoint.t ]
        type infinite = [ `Until_infinity ]
        type +'a range = private { until : 'a; }
          constraint 'a = [< `Before of Endpoint.t | `Until_infinity ]
        val until :
          ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
      end
  end
|}]
