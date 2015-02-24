open Cmm

let rec of_expression = function
  | Cvar _
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _ -> []
  | Cconst_symbol sym -> [ `Direct_call sym ]
  | Cconst_pointer _ -> [] (* CR trefis: TODO? *)
  | Cconst_natpointer _ -> [] (* CR trefis: TODO? *)
  | Cconst_blockheader _ -> [] (* CR trefis: TODO? *)

  | Clet (id1, Cconst_symbol sym,
          Cop (Cstore _, [Cconst_symbol prefix ; Cvar id2]))
    when Ident.same id1 id2 ->
      let prelen = String.length prefix in
      if String.length sym > prelen && String.sub sym 0 prelen = prefix then
        (* Typical initialization pattern, ignore *)
        let () = Printf.printf "IGNORING %s, USED IN INIT\n" sym in
        []
      else
        [ `Direct_call sym ; `Field_access (prefix, 0) ]

  | Clet (id1, Cconst_symbol sym,
          Cop (Cstore _,
               [ Cop (Cadda, [ Cconst_symbol prefix ; Cconst_int offset]) ;
                 Cvar id2 ]))
    when Ident.same id1 id2 ->
      let prelen = String.length prefix in
      if String.length sym > prelen && String.sub sym 0 prelen = prefix then
        (* Typical initialization pattern, ignore *)
        let () = Printf.printf "IGNORING %s, USED IN INIT\n" sym in
        []
      else
        [ `Direct_call sym ; `Field_access (prefix, offset / 8) ]

  | Ctrywith (e1, _, e2)
  | Ccatch (_,_, e1, e2)
  | Csequence (e1,e2)
  | Clet (_, e1, e2) -> of_expression e1 @ of_expression e2

  | Cloop e
  | Cassign (_, e) -> of_expression e

  | Cexit (_, lst)
  | Ctuple lst -> List.concat (List.map of_expression lst)


  | Cop (Cadda, [ Cconst_symbol sym ; Cconst_int offset ]) ->
      [ `Field_access (sym, offset / 8) ]

  | Cop (Cstore _, (Cconst_symbol sym) :: _)
  | Cop (Cload _ , (Cconst_symbol sym) :: _) ->
      [ `Field_access (sym, 0) ]

  | Cop (_, lst) ->
    List.concat (List.map of_expression lst)

  | Cifthenelse (e1, e2, e3) ->
      of_expression e1 @ of_expression e2 @ of_expression e3

  | Cswitch (e1, _, es) ->
      List.concat (
        of_expression e1 ::
        Array.fold_left (fun lst e -> of_expression e :: lst) [] es
      )

module IntSet = Set.Make (struct
  type t = int
  let compare (x:int) (y:int) = compare x y
end)

(* We need to handle "__entry" functions specially, as they reference all the
   closures created for functions of that module. *)

let of_fundecl fdecl =
  let fname = fdecl.fun_name in
  let dependencies = of_expression fdecl.fun_body in
  let compare a b =
    match a, b with
    | `Direct_call s1, `Direct_call s2 -> String.compare s1 s2
    | `Direct_call _, _ -> -1
    | _, `Direct_call _ -> 1
    | `Field_access (unit1, off1), `Field_access (unit2, off2) ->
        match String.compare unit1 unit2 with
        | 0 -> compare (off1 : int) off2
        | n -> n
  in
  let dependencies = List.sort_uniq compare dependencies in
  Compilenv.record_dependencies fname dependencies

(*
let of_data_items items =
  List.iter (function
    | Csymbol_address sym -> `Dir
  ) items
*)

let of_phrase = function
  | Cdata data_items -> () (* CR trefis: FIXME? *)
  | Cfunction fdecl -> of_fundecl fdecl
