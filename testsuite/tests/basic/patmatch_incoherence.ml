(* TEST
   flags = "-drawlambda"
   * expect
*)

type tlist = { x: 'a. 'a list };;
[%%expect{|
0a
type tlist = { x : 'a. 'a list; }
|}];;

match { x = [] } with
| { x = [] } -> ()
| { x = 3 :: _ } -> ()
| { x = "" :: _ } -> ()
;;
[%%expect{|
(let (*match*/1016 = [0: 0a] *match*/1017 =a (field 0 *match*/1016))
  (if *match*/1017
    (let (*match*/1018 =a (field 0 *match*/1017))
      (raise (makeblock 0 (global Match_failure/18g) [0: "_none_" 1 -1])))
    (before (2):177-179 0a)))
- : unit = ()
|}];;


type t = { x: 'a. 'a };;
[%%expect{|
0a
type t = { x : 'a. 'a; }
|}];;

match { x = assert false } with
| { x = 3 } -> ()
| { x = "" } -> ()
;;
[%%expect{|
(let
  (*match*/1026 =
     (makeblock 0
       (raise
         (after (1):591-603
           (makeblock 0 (global Assert_failure/28g) [0: "" 1 12]))))
   *match*/1027 =a (field 0 *match*/1026))
  (raise (makeblock 0 (global Match_failure/18g) [0: "_none_" 1 -1])))
Exception: Assert_failure ("", 1, 12).
|}, Principal{|
(let
  (*match*/1026 =
     (makeblock 0
       (raise
         (after (1):864-876
           (makeblock 0 (global Assert_failure/28g) [0: "" 1 12]))))
   *match*/1027 =a (field 0 *match*/1026))
  (raise (makeblock 0 (global Match_failure/18g) [0: "_none_" 1 -1])))
Exception: Assert_failure ("", 1, 12).
|}];;

match { x = assert false } with
| { x = (2., "") } -> ()
| { x = None } -> ()
| { x = 3 } -> ()
;;
[%%expect{|
Line _, characters 0-95:
  match { x = assert false } with
  | { x = (2., "") } -> ()
  | { x = None } -> ()
  | { x = 3 } -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=0}
(catch
  (let
    (*match*/1033 =
       (makeblock 0
         (raise
           (after (1):987-999
             (makeblock 0 (global Assert_failure/28g) [0: "" 1 12]))))
     *match*/1034 =a (field 0 *match*/1033)
     *match*/1035 =a (field 0 *match*/1034))
    (if (!=. *match*/1035 2.) (exit 1)
      (let (*match*/1036 =a (field 1 *match*/1034))
        (stringswitch *match*/1036
         case "": (before (2):1029-1031 0a)
         default: (exit 1)))))
 with (1) (raise (makeblock 0 (global Match_failure/18g) [0: "" 1 0])))
Exception: Assert_failure ("", 1, 12).
|}, Principal{|
Line _, characters 0-95:
  match { x = assert false } with
  | { x = (2., "") } -> ()
  | { x = None } -> ()
  | { x = 3 } -> ()
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
{x=0}
(catch
  (let
    (*match*/1033 =
       (makeblock 0
         (raise
           (after (1):1581-1593
             (makeblock 0 (global Assert_failure/28g) [0: "" 1 12]))))
     *match*/1034 =a (field 0 *match*/1033)
     *match*/1035 =a (field 0 *match*/1034))
    (if (!=. *match*/1035 2.) (exit 1)
      (let (*match*/1036 =a (field 1 *match*/1034))
        (stringswitch *match*/1036
         case "": (before (2):1623-1625 0a)
         default: (exit 1)))))
 with (1) (raise (makeblock 0 (global Match_failure/18g) [0: "" 1 0])))
Exception: Assert_failure ("", 1, 12).
|}];;
