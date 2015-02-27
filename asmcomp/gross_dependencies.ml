open Cmm

let unit_symbol = ref ""

let is_local_store id = function
  | Cop (Cstore _, [Cconst_symbol sym ; Cvar id'])
  | Cop (Cstore _, [ Cop (Cadda, [ Cconst_symbol sym ; Cconst_int _ ]) ;
                     Cvar id' ]) ->
      Ident.same id id' && sym = !unit_symbol
  | _ -> false

let rec of_expression ppf = function
  | Cvar _
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_pointer _
  | Cconst_natpointer _
  | Cconst_blockheader _ -> []

  | Cconst_symbol sym -> [ `Direct_call sym ]

  | Clet (id, Cconst_symbol sym, subtree)
    when is_local_store id subtree ->
      (* Typical initialization pattern, ignore *)
      if !Clflags.dump_unused then
        Format.fprintf ppf "IGNORING %s, USED IN %s__entry\n" sym !unit_symbol;
      []

  | Ctrywith (e1, _, e2)
  | Ccatch (_,_, e1, e2)
  | Csequence (e1,e2)
  | Clet (_, e1, e2) -> of_expression ppf e1 @ of_expression ppf e2

  | Cloop e
  | Cassign (_, e) -> of_expression ppf e

  | Cexit (_, lst)
  | Ctuple lst -> List.concat (List.map (of_expression ppf) lst)


  | Cop (Cadda, [ Cconst_symbol sym ; Cconst_int offset ]) ->
      [ `Field_access (sym, offset / 8) ]

  | Cop (Cstore _, (Cconst_symbol sym) :: _)
  | Cop (Cload _ , (Cconst_symbol sym) :: _) ->
      [ `Field_access (sym, 0) ]

  | Cop (_, lst) ->
    List.concat (List.map (of_expression ppf) lst)

  | Cifthenelse (e1, e2, e3) ->
      of_expression ppf e1 @ of_expression ppf e2 @ of_expression ppf e3

  | Cswitch (e1, _, es) ->
      List.concat (
        of_expression ppf e1 ::
        Array.fold_left (fun lst e -> of_expression ppf e :: lst) [] es
      )

module IntSet = Set.Make (struct
  type t = int
  let compare (x:int) (y:int) = compare x y
end)

let of_fundecl ppf fdecl =
  unit_symbol := Compilenv.make_symbol None ;
  let fname = fdecl.fun_name in
  (* We need to handle "__entry" functions specially, as they reference all the
     closures created for functions of that module. *)
  let dependencies = of_expression ppf fdecl.fun_body in
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

let of_phrase ppf = function
  | Cdata data_items -> () (* CR trefis: FIXME? *)
  | Cfunction fdecl -> of_fundecl ppf fdecl
