(* TEST
   * expect
*)

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end;;
[%%expect{|
module type Monad =
  sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end
|}];;

let return {M : Monad} x = M.return x;;
[%%expect{|
val return : {M: Monad} -> 'a -> 'a M.t = <fun>
|}];;

let (>>=) {M : Monad} m k = M.bind m k;;
[%%expect{|
val ( >>= ) : {M: Monad} -> 'a M.t -> ('a -> 'b M.t) -> 'b M.t = <fun>
|}];;

implicit module ListMonad = struct
  type 'a t = 'a list
  let return x = [x]
  let bind x f =
    let rec aux acc = function
      | x :: xs -> aux (x @ acc) xs
      | [] -> acc in
    aux [] (List.rev_map f x)
end;;
[%%expect{|
module ListMonad :
  sig
    type 'a t = 'a list
    val return : 'a -> 'a list
    val bind : 'a list -> ('a -> 'b list) -> 'b list
  end
|}];;

implicit module OptionMonad = struct
  type 'a t = 'a option
  let return x = Some x
  let bind x f = match x with
    | None -> None
    | Some x -> f x
end;;
[%%expect{|
module OptionMonad :
  sig
    type 'a t = 'a option
    val return : 'a -> 'a option
    val bind : 'a option -> ('a -> 'b option) -> 'b option
  end
|}];;

(* FIXME: in the toplevel, all of these give me different results than here.
   (and only the last one appears "broken"). *)

(* Ambiguous *)
let a = (return 5) >>= fun x -> return x;;
[%%expect{|
Line 1, characters 19-22:
1 | let a = (return 5) >>= fun x -> return x;;
                       ^^^
Error: No instance found for implicit M.
|}];;

let l : 'a list = (return 5) >>= fun x -> return x;;
[%%expect{|
Line 1, characters 29-32:
1 | let l : 'a list = (return 5) >>= fun x -> return x;;
                                 ^^^
Error: No instance found for implicit M.
|}];;

let o : 'a option = (return 5) >>= fun x -> return x;;
[%%expect{|
Line 1, characters 31-34:
1 | let o : 'a option = (return 5) >>= fun x -> return x;;
                                   ^^^
Error: No instance found for implicit M.
|}];;

let m = [1; 2; 3] >>= fun x -> return x;;
[%%expect{|
Line 1, characters 18-21:
1 | let m = [1; 2; 3] >>= fun x -> return x;;
                      ^^^
Error: No instance found for implicit M.
|}];;

let n = return 5 >>= fun x -> [x; 2; 3];;
[%%expect{|
Line 1, characters 17-20:
1 | let n = return 5 >>= fun x -> [x; 2; 3];;
                     ^^^
Error: No instance found for implicit M.
|}];;

(* Various implementations of sequence to test the handling of recursion *)
let rec sequence : {M : Monad} -> 'a M.t list -> 'a list M.t =
  fun {M : Monad} (x : 'a M.t list) ->
    match x with
    | [] -> (return [] : 'a list M.t)
    | x :: xs ->
        x >>= fun y ->
        sequence xs >>= fun ys ->
          return (y :: ys);;
[%%expect{|
Lines 1-8, characters 8-26:
1 | ........sequence : {M : Monad} -> 'a M.t list -> 'a list M.t =
2 |   fun {M : Monad} (x : 'a M.t list) ->
3 |     match x with
4 |     | [] -> (return [] : 'a list M.t)
5 |     | x :: xs ->
6 |         x >>= fun y ->
7 |         sequence xs >>= fun ys ->
8 |           return (y :: ys)..
Error: This expression has type
         {M/1: Monad} -> 'a M/1.t list -> 'a list M/1.t
       but an expression was expected of type
         {M/3: Monad} -> 'a M/2.t list -> 'a list M/2.t
       Type 'a M/1.t is not compatible with type 'a M/2.t
|}, Principal{|
val sequence : {M: Monad} -> 'a M.t list -> 'a list M.t = <fun>
|}];;

let rec sequence : type a. {M : Monad} -> a M.t list -> a list M.t =
  fun {M : Monad} (x : a M.t list) ->
    match x with
    | [] -> (return [] : a list M.t)
    | x :: xs ->
        x >>= fun y ->
        sequence xs >>= fun ys ->
          return (y :: ys);;
[%%expect{|
Lines 1-8, characters 8-26:
1 | ........sequence : type a. {M : Monad} -> a M.t list -> a list M.t =
2 |   fun {M : Monad} (x : a M.t list) ->
3 |     match x with
4 |     | [] -> (return [] : a list M.t)
5 |     | x :: xs ->
6 |         x >>= fun y ->
7 |         sequence xs >>= fun ys ->
8 |           return (y :: ys)..
Error: The universal type variable 'a cannot be generalized:
       it escapes its scope.
|}];;

let rec sequence : {M : Monad} -> 'a M.t list -> 'a list M.t =
  fun {M : Monad} x ->
    match x with
    | [] -> return []
    | x :: xs ->
        x >>= fun y ->
        sequence xs >>= fun ys ->
          return (y :: ys);;
[%%expect{|
Lines 1-8, characters 8-26:
1 | ........sequence : {M : Monad} -> 'a M.t list -> 'a list M.t =
2 |   fun {M : Monad} x ->
3 |     match x with
4 |     | [] -> return []
5 |     | x :: xs ->
6 |         x >>= fun y ->
7 |         sequence xs >>= fun ys ->
8 |           return (y :: ys)..
Error: This expression has type
         {M/1: Monad} -> 'a M/1.t list -> 'a list M/1.t
       but an expression was expected of type
         {M/3: Monad} -> 'a M/2.t list -> 'a list M/2.t
       Type 'a M/1.t is not compatible with type 'a M/2.t
|}, Principal{|
val sequence : {M: Monad} -> 'a M.t list -> 'a list M.t = <fun>
|}];;

let rec sequence : 'a. {M : Monad} -> 'a M.t list -> 'a list M.t =
  fun {M : Monad} x ->
    match x with
    | [] -> return []
    | x :: xs ->
        x >>= fun y ->
        sequence xs >>= fun ys ->
          return (y :: ys);;
[%%expect{|
Line 1, characters 19-64:
1 | let rec sequence : 'a. {M : Monad} -> 'a M.t list -> 'a list M.t =
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized:
       it escapes its scope.
|}];;

let rec sequence : type a. {M : Monad} -> a M.t list -> a list M.t =
  fun {M : Monad} x ->
    match x with
    | [] -> return []
    | x :: xs ->
        x >>= fun y ->
        sequence xs >>= fun ys ->
          return (y :: ys);;
[%%expect{|
Lines 1-8, characters 8-26:
1 | ........sequence : type a. {M : Monad} -> a M.t list -> a list M.t =
2 |   fun {M : Monad} x ->
3 |     match x with
4 |     | [] -> return []
5 |     | x :: xs ->
6 |         x >>= fun y ->
7 |         sequence xs >>= fun ys ->
8 |           return (y :: ys)..
Error: The universal type variable 'a cannot be generalized:
       it escapes its scope.
|}];;

let rec sequence : {M : Monad} -> 'a M.t list -> 'a list M.t =
  fun {M : Monad} x ->
    match x with
    | [] -> M.return []
    | x :: xs ->
        M.bind x (fun y ->
        M.bind (sequence xs) (fun ys ->
          M.return (y :: ys)));;
[%%expect{|
Lines 1-8, characters 8-30:
1 | ........sequence : {M : Monad} -> 'a M.t list -> 'a list M.t =
2 |   fun {M : Monad} x ->
3 |     match x with
4 |     | [] -> M.return []
5 |     | x :: xs ->
6 |         M.bind x (fun y ->
7 |         M.bind (sequence xs) (fun ys ->
8 |           M.return (y :: ys)))..
Error: This expression has type
         {M/1: Monad} -> 'a M/1.t list -> 'a list M/1.t
       but an expression was expected of type
         {M/3: Monad} -> 'a M/2.t list -> 'a list M/2.t
       Type 'a M/1.t is not compatible with type 'a M/2.t
|}, Principal{|
val sequence : {M: Monad} -> 'a M.t list -> 'a list M.t = <fun>
|}];;

let rec sequence : 'a. {M : Monad} -> 'a M.t list -> 'a list M.t =
  fun {M : Monad} x ->
    match x with
    | [] -> M.return []
    | x :: xs ->
        M.bind x (fun y ->
        M.bind (sequence xs) (fun ys ->
          M.return (y :: ys)));;
[%%expect{|
Line 1, characters 19-64:
1 | let rec sequence : 'a. {M : Monad} -> 'a M.t list -> 'a list M.t =
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized:
       it escapes its scope.
|}];;

let rec sequence : type a. {M : Monad} -> a M.t list -> a list M.t =
  fun {M : Monad} x ->
    match x with
    | [] -> M.return []
    | x :: xs ->
        M.bind x (fun y ->
        M.bind (sequence xs) (fun ys ->
          M.return (y :: ys)));;
[%%expect{|
Lines 1-8, characters 8-30:
1 | ........sequence : type a. {M : Monad} -> a M.t list -> a list M.t =
2 |   fun {M : Monad} x ->
3 |     match x with
4 |     | [] -> M.return []
5 |     | x :: xs ->
6 |         M.bind x (fun y ->
7 |         M.bind (sequence xs) (fun ys ->
8 |           M.return (y :: ys)))..
Error: The universal type variable 'a cannot be generalized:
       it escapes its scope.
|}];;
