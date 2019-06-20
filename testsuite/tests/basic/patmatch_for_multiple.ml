(* TEST
   flags = "-drawlambda"
   * expect
*)

(* Successful flattening *)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let (*match*/87 = 3 *match*/88 = 2 *match*/89 = 1)
  (catch
    (catch
      (catch (if (!= *match*/87 1) (exit 3) (exit 1)) with (3)
        (if (!= *match*/88 3) (exit 2) (exit 1)))
     with (2) 0a)
   with (1) 1a))
- : bool = false
|}];;

(* Failed flattening: we need to allocate the tuple to bind x. *)

match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let
  (*match*/92 = 3
   *match*/93 = 2
   *match*/94 = 1
   *match*/95 = (makeblock 0 *match*/92 *match*/93 *match*/94))
  (catch
    (let (*match*/96 =a (field 0 *match*/95))
      (catch
        (catch
          (if (!= *match*/96 1) (exit 7)
            (let
              (*match*/98 =a (field 2 *match*/95)
               *match*/97 =a (field 1 *match*/95))
              (exit 5 *match*/95)))
         with (7)
          (let (*match*/99 =a (field 1 *match*/95))
            (if (!= *match*/99 3) (exit 6)
              (let (*match*/100 =a (field 2 *match*/95)) (exit 5 *match*/95)))))
       with (6)
        (let
          (*match*/102 =a (field 2 *match*/95)
           *match*/101 =a (field 1 *match*/95))
          0a)))
   with (5 x/90) (seq (ignore x/90) 1a)))
- : bool = false
|}];;

