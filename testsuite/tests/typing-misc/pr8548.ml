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
Line 1:
Error: Module type declarations do not match:
         module type Ranged =
           sig
             module Endpoint : Endpoint_intf
             module Range :
               sig
                 module Endpoint : sig type t = Endpoint.t end
                 type finite = [ `Before of Endpoint.t ]
                 type infinite = [ `Until_infinity ]
                 type +'a range = private { until : 'a; }
                   constraint 'a =
                     [< `Before of Endpoint.t | `Until_infinity ]
                 val until :
                   ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range ->
                   'a
               end
           end
       does not match
         module type Ranged =
           sig
             module Endpoint : Endpoint_intf
             module Range :
               sig
                 module Endpoint : sig type t = Endpoint.t end
                 type finite = [ `Before of Endpoint.t ]
                 type infinite = [ `Until_infinity ]
                 type +'a range = private { until : 'a; }
                   constraint 'a =
                     [< `Before of Endpoint.t | `Until_infinity ]
                 val until :
                   ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range ->
                   'a
               end
           end
       At position module type Ranged = <here>
       Modules do not match:
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
                 ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range ->
                 'a
             end
         end
       is not included in
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
                 ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range ->
                 'a
             end
         end
       At position module type Ranged = sig module Range : <here> end
       Modules do not match:
         sig
           module Endpoint = Range.Endpoint
           type finite = [ `Before of Endpoint.t ]
           type infinite = [ `Until_infinity ]
           type +'a range = 'a Range.range = private { until : 'a; }
             constraint 'a = [< `Before of Endpoint.t | `Until_infinity ]
           val until :
             ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
         end
       is not included in
         sig
           module Endpoint : sig type t = Endpoint.t end
           type finite = [ `Before of Endpoint.t ]
           type infinite = [ `Until_infinity ]
           type +'a range = private { until : 'a; }
             constraint 'a = [< `Before of Endpoint.t | `Until_infinity ]
           val until :
             ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
         end
       At position module type Ranged = sig module Range : <here> end
       Values do not match:
         val until :
           ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
       is not included in
         val until :
           ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
|}]
