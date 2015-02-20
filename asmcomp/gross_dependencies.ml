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

  | Ctrywith (e1, _, e2)
  | Ccatch (_,_, e1, e2)
  | Csequence (e1,e2)
  | Clet (_, e1, e2) -> of_expression e1 @ of_expression e2

  | Cloop e
  | Cassign (_, e) -> of_expression e

  | Cexit (_, lst)
  | Ctuple lst -> List.concat (List.map of_expression lst)


  | Cop (Cadda, [ Cconst_symbol sym ; Cconst_int offset ]) ->
      [ `Field_access (sym, offset) ]

  | Cop (Cstore _, (Cconst_symbol sym) :: _)
  | Cop (Cload _ , (Cconst_symbol sym) :: _) ->
      [ `Field_access (sym, 0) ]

  | Cop (_, lst) ->
    (* CR trefis: TODO: we also want to look at the actual operation.
        If we have e.g.[Cop (Cadda, [ Cconst_symbol "camlFoo" ; Cconst_int 8 ])]
        and the second field of [Foo] is also a function. 
    *)
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
  let fnlen = String.length fname in
  let dependencies = of_expression fdecl.fun_body in
  let dependencies =
    if fnlen < 7 || String.sub fname (fnlen - 7) 7 <> "__entry" then
      dependencies
    else
    (* Yum *)
    let current_unit_name = "caml" ^ Compilenv.current_unit_name () in
    let internal_closures_names =
      List.map (fun (n,_,_) -> n)
        (Compilenv.current_unit_infos ()).Cmx_format.ui_const_closures
    in
    let _, dependencies =
      List.fold_left (fun (local_fields, real_dependencies) dep ->
        match dep with
        | `Field_access (unit_name, offset) ->
          if unit_name = current_unit_name then
            if IntSet.mem offset local_fields then
              (* We already set that field, so this access must be a real
                dependency. *)
              (local_fields, dep :: real_dependencies)
            else
              (* We haven't seen that field yet, this means that we are merely
                initializing the structure. There is no real dependency here (i.e.
                that field is not "used"). *)
              (IntSet.add offset local_fields, real_dependencies)
          else
            (local_fields, dep :: real_dependencies)
        | `Direct_call sym ->
            (* That case is more obvious. The only places where internal
               closures are referenced are when we store them.
               If they are needed, a field access is emited. *)
            if List.mem sym internal_closures_names then
              (local_fields, real_dependencies)
            else
              (local_fields, dep :: real_dependencies)
      ) (IntSet.empty, []) dependencies
    in
    dependencies
  in
  Compilenv.record_dependencies fname dependencies

let of_phrase = function
  | Cdata _ -> () (* CR trefis: FIXME? *)
  | Cfunction fdecl -> of_fundecl fdecl
