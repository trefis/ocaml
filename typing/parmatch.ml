(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Detection of partial matches and unused match cases. *)

open Misc
open Asttypes
open Types
open Typedtree

(*******************)
(* Simple patterns *)
(*******************)

module IntSet = Set.Make(struct
    type t = int
    let compare : int -> int -> int = Pervasives.compare
  end)

module Simple_pattern = struct
  type head =
    | Sany
    | Sarray of int
    | Sconstant of Asttypes.constant
    | Sconstruct of constructor_description
    | Slazy
    | Srecord of {
        closed : closed_flag;
        all_labels : label_description array;
        (* FIXME: perhaps an [(Longident.t loc) IntMap.t] would be nicer here. *)
        mutable fields : IntSet.t
      }
    | Stuple of int
    | Svariant of { name : label; has_argument : bool; row : row_desc ref }

  type t = {
    sp_head : head;
    sp_type : Types.type_expr;
    sp_env : Env.t;
  }
end

open Simple_pattern

(*************************************)
(* Utilities for building patterns   *)
(*************************************)

let make_pat desc ty tenv =
  {pat_desc = desc; pat_loc = Location.none; pat_extra = [];
   pat_type = ty ; pat_env = tenv;
   pat_attributes = [];
  }

let omega = make_pat Tpat_any Ctype.none Env.empty

let extra_pat =
  make_pat
    (Tpat_var (Ident.create "+", mknoloc "+"))
    Ctype.none Env.empty

let rec omegas i =
  if i <= 0 then [] else omega :: omegas (i-1)

let omega_list l = List.map (fun _ -> omega) l

let zero = make_pat (Tpat_constant (Const_int 0)) Ctype.none Env.empty

(***********************)
(* Compatibility check *)
(***********************)

(* p and q compatible means, there exists V that matches both *)

let is_absent tag row = Btype.row_field tag !row = Rabsent

let is_absent_pat sp = match sp.sp_head with
| Svariant { name; row; _ } -> is_absent name row
| _ -> false

let const_compare x y =
  match x,y with
  | Const_float f1, Const_float f2 ->
      Pervasives.compare (float_of_string f1) (float_of_string f2)
  | Const_string (s1, _), Const_string (s2, _) ->
      String.compare s1 s2
  | _, _ -> Pervasives.compare x y

let records_args l1 l2 =
  (* Invariant: fields are already sorted by Typecore.type_label_a_list *)
  let rec combine r1 r2 l1 l2 = match l1,l2 with
  | [],[] -> List.rev r1, List.rev r2
  | [],(_,_,p2)::rem2 -> combine (omega::r1) (p2::r2) [] rem2
  | (_,_,p1)::rem1,[] -> combine (p1::r1) (omega::r2) rem1 []
  | (_,lbl1,p1)::rem1, ( _,lbl2,p2)::rem2 ->
      if lbl1.lbl_pos < lbl2.lbl_pos then
        combine (p1::r1) (omega::r2) rem1 l2
      else if lbl1.lbl_pos > lbl2.lbl_pos then
        combine (omega::r1) (p2::r2) l1 rem2
      else (* same label on both sides *)
        combine (p1::r1) (p2::r2) rem1 rem2 in
  combine [] [] l1 l2


let rec compat p q =
  match p.pat_desc,q.pat_desc with
  | Tpat_alias (p,_,_),_      -> compat p q
  | _,Tpat_alias (q,_,_)      -> compat p q
  | (Tpat_any|Tpat_var _),_ -> true
  | _,(Tpat_any|Tpat_var _) -> true
  | Tpat_or (p1,p2,_),_     -> compat p1 q || compat p2 q
  | _,Tpat_or (q1,q2,_)     -> compat p q1 || compat p q2
  | Tpat_constant c1, Tpat_constant c2 -> const_compare c1 c2 = 0
  | Tpat_tuple ps, Tpat_tuple qs -> compats ps qs
  | Tpat_lazy p, Tpat_lazy q -> compat p q
  | Tpat_construct (_, c1,ps1), Tpat_construct (_, c2,ps2) ->
      Types.equal_tag c1.cstr_tag c2.cstr_tag && compats ps1 ps2
  | Tpat_variant(l1,Some p1, _r1), Tpat_variant(l2,Some p2,_) ->
      l1=l2 && compat p1 p2
  | Tpat_variant (l1,None, _r1), Tpat_variant(l2,None,_) ->
      l1 = l2
  | Tpat_variant (_, None, _), Tpat_variant (_,Some _, _) -> false
  | Tpat_variant (_, Some _, _), Tpat_variant (_, None, _) -> false
  | Tpat_record (l1,_),Tpat_record (l2,_) ->
      let ps,qs = records_args l1 l2 in
      compats ps qs
  | Tpat_array ps, Tpat_array qs ->
      List.length ps = List.length qs &&
      compats ps qs
  | _,_  ->
      assert false

and compats ps qs = match ps,qs with
| [], [] -> true
| p::ps, q::qs -> compat p q && compats ps qs
| _,_    -> assert false

exception Empty (* Empty pattern *)

(****************************************)
(* Utilities for retrieving type paths  *)
(****************************************)

(* May need a clean copy, cf. PR#4745 *)
let clean_copy ty =
  if ty.level = Btype.generic_level then ty
  else Subst.type_expr Subst.identity ty

(* As reported in PR#6394 it is possible for recursive modules to add incoherent
   equations into the environment.
   So assuming we're working on the same example as in PR#6394, when looking at
   constructor [A] we end up calling [expand_head] on [t] (the type of [A]) and
   get [int * bool].
   This will result in a proper error later on during time checking, meanwhile
   we need to "survive" and be somewhat aware that we're working on bogus input
*)

type constructor_type_path =
  | Ok of Path.t
  | Inconsistent_environment

let get_constructor_type_path ty tenv =
  let ty = Ctype.repr (Ctype.expand_head tenv (clean_copy ty)) in
  match ty.desc with
  | Tconstr (path,_,_) -> Ok path
  | _ -> Inconsistent_environment

(****************************)
(* Utilities for matching   *)
(****************************)


(* Check top matching *)
let simple_match sp p2 =
  match sp, p2.pat_desc with
  | Sconstruct cd, Tpat_construct(_, c2, _) ->
      Types.equal_tag cd.cstr_tag c2.cstr_tag
  | Svariant { name = l1; _ }, Tpat_variant(l2, _, _) ->
      l1 = l2
  | Sconstant(c1), Tpat_constant(c2) -> const_compare c1 c2 = 0
  | Stuple _, Tpat_tuple _ -> true
  | Slazy, Tpat_lazy _ -> true
  | Srecord _ , Tpat_record _ -> true
  | Sarray length, Tpat_array p2s -> length = List.length p2s
  | _, (Tpat_any | Tpat_var(_)) -> true
  | _, _ -> false

(* Raise Not_found when pos is not present in arg *)
let get_field pos args =
  let _,_, p = List.find (fun (_,lbl,_) -> pos = lbl.lbl_pos) args in
  p

let extract_fields sp args =
  match sp with
  | Sany -> []
  | Srecord { fields; _ } ->
    List.map (fun lbl_pos ->
      try get_field lbl_pos args
      with Not_found -> omega
    ) (IntSet.elements fields)
  | _ -> fatal_error "Parmatch.extract_fields"

(* Build argument list when p2 >= sp *)
let rec simple_match_args sp p2 = match p2.pat_desc with
| Tpat_alias (p2,_,_) -> simple_match_args sp p2
| Tpat_construct(_, _, args) -> args
| Tpat_variant(_, Some arg, _) -> [arg]
| Tpat_tuple(args)  -> args
| Tpat_record(args,_) -> extract_fields sp args
| Tpat_array(args) -> args
| Tpat_lazy arg -> [arg]
| Tpat_any
| Tpat_var(_) ->
    begin match sp with
    | Sany
    | Svariant { has_argument = false; _ } -> []
    | Sconstant _
    | Slazy
    | Svariant _ -> [omega]
    | Stuple arity
    | Sarray arity
    | Sconstruct { cstr_arity = arity } -> omegas arity
    | Srecord {fields; _} -> omegas (IntSet.cardinal fields)
    end
| _ -> []

let row_of_type env typ =
  match Ctype.expand_head env typ with
    {desc = Tvariant row} -> Btype.row_repr row
  | _ -> assert false

(*
  Normalize a pattern ->
   all arguments are omega (simple pattern) and no more variables
*)

let rec normalize_pat q =
  let mk_simple_pat sp_head =
    { sp_head; sp_type = q.pat_type; sp_env = q.pat_env }
  in
  match q.pat_desc with
  | Tpat_any
  | Tpat_var _ -> mk_simple_pat Sany
  | Tpat_constant c -> mk_simple_pat (Sconstant c)
  | Tpat_alias (p,_,_) -> normalize_pat p
  | Tpat_tuple args -> mk_simple_pat (Stuple (List.length args))
  | Tpat_construct  (_, cd, _) ->
    mk_simple_pat (Sconstruct cd)
  | Tpat_variant (l, arg, row) ->
    mk_simple_pat (Svariant { name = l; has_argument = not (arg = None); row })
  | Tpat_array args -> mk_simple_pat (Sarray (List.length args))
  | Tpat_record (largs, closed) ->
    let all_labels =
      match largs with
      | (_, lbl, _) :: _ -> lbl.lbl_all
      | _ -> assert false
    in
    let fields =
      List.fold_left (fun set (_, lbl, _) ->
        IntSet.add lbl.lbl_pos set
      ) IntSet.empty largs
    in
    mk_simple_pat (Srecord { closed; fields; all_labels })
  | Tpat_lazy _ -> mk_simple_pat Slazy
  | Tpat_or _ -> fatal_error "Parmatch.normalize_pat (Tpat_or _)"

(* Consider a pattern matrix whose first column has been simplified
   to contain only _ or a head constructor
     | p1, r1...
     | p2, r2...
     | p3, r3...
     | ...

   We build a normalized /discriminating/ pattern from a pattern [q] by folding
   over the first column of the matrix, "refining" [q] as we go:

   - when we encounter a row starting with [Tpat_tuple] or [Tpat_lazy] then we
   can stop and return that pattern, as we cannot refine any further. Indeed,
   these constructors are alone in their signature, so they will subsume
   whatever other pattern we might find, as well as the pattern we're threading
   along.

   - when we find a [Tpat_record] then it is a bit more involved: it is also
   alone in its signature, however it might only be matching a subset of the
   record fields. We use these fields to refine our accumulator and keep going
   as another row might match on different fields.

   - rows starting with a wildcard do not bring any information, so we ignore
   them and keep going

   - if we encounter anything else (i.e. any other constructor), then we just
   stop and return our accumulator.
*)
let discr_pat q pss =
  let rec refine_pat acc = function
    | [] -> acc
    | (head, _) :: rows ->
      match head.pat_desc with
      | Tpat_or _ | Tpat_var _ | Tpat_alias _ -> assert false
      | Tpat_any -> refine_pat acc rows
      | Tpat_tuple _ | Tpat_lazy _ -> normalize_pat head
      | Tpat_record (largs, _) ->
        let new_acc =
          match acc.sp_head with
          | Sany -> normalize_pat head
          | Srecord t ->
            let fields =
              List.fold_left (fun set (_, lbl, _) -> IntSet.add lbl.lbl_pos set)
                t.fields largs
            in
            t.fields <- fields;
            acc
          | _ -> assert false
        in
        refine_pat new_acc rows
      | _ -> acc
  in
  let q = normalize_pat q in
  (* short-circuiting: clearly if we have anything other than [Tpat_record] or
     [Tpat_any] to start with, we're not going to be able refine at all. So
     there's no point going over the matrix. *)
  match q.sp_head with
  | Sany | Srecord _ -> refine_pat q pss
  | _ -> q

(*
   In case a matching value is found, set actual arguments
   of the matching pattern.
*)

let rec read_args arity r = match arity,r with
| 0,_ -> [],r
| n, arg::rest ->
   let args,rest = read_args (n - 1) rest in
   arg::args,rest
| _,_ ->
    fatal_error "Parmatch.read_args"

let mknoloclid s = mknoloc (Longident.Lident s)

let pat_of_sp sp =
  let pat_desc =
    match sp.sp_head with
    | Sany -> Tpat_any
    | Sarray n -> Tpat_array (omegas n)
    | Sconstant c -> Tpat_constant c
    | Sconstruct c ->
      Tpat_construct (mknoloclid c.cstr_name, c, omegas c.cstr_arity)
    | Slazy -> Tpat_lazy omega
    | Srecord { closed; all_labels; fields } ->
      let lbls =
        IntSet.fold (fun lbl_pos acc ->
          let lbl = all_labels.(lbl_pos) in
          (* NB. we're losing elements of the path here *)
          (mknoloclid lbl.lbl_name, lbl, omega) :: acc
        ) fields []
      in
      Tpat_record (lbls, closed)
    | Stuple n -> Tpat_tuple (omegas n)
    | Svariant { name; has_argument; row } ->
      Tpat_variant (name, (if has_argument then Some omega else None), row)
  in
  { pat_desc
  ; pat_env = sp.sp_env
  ; pat_type = sp.sp_type
  ; pat_loc = Location.none
  ; pat_extra = []
  ; pat_attributes = [] }

(*
let do_set_args erase_mutable q r = match q with
| {pat_desc = Tpat_tuple omegas} ->
    let args,rest = read_args omegas r in
    make_pat (Tpat_tuple args) q.pat_type q.pat_env::rest
| {pat_desc = Tpat_record (omegas,closed)} ->
    let args,rest = read_args omegas r in
    make_pat
      (Tpat_record
         (List.map2 (fun (lid, lbl,_) arg ->
           if
             erase_mutable &&
             (match lbl.lbl_mut with
             | Mutable -> true | Immutable -> false)
           then
             lid, lbl, omega
           else
             lid, lbl, arg)
            omegas args, closed))
      q.pat_type q.pat_env::
    rest
| {pat_desc = Tpat_construct (lid, c,omegas)} ->
    let args,rest = read_args omegas r in
    make_pat
      (Tpat_construct (lid, c,args))
      q.pat_type q.pat_env::
    rest
| {pat_desc = Tpat_variant (l, omega, row)} ->
    let arg, rest =
      match omega, r with
        Some _, a::r -> Some a, r
      | None, r -> None, r
      | _ -> assert false
    in
    make_pat
      (Tpat_variant (l, arg, row)) q.pat_type q.pat_env::
    rest
| {pat_desc = Tpat_lazy _omega} ->
    begin match r with
      arg::rest ->
        make_pat (Tpat_lazy arg) q.pat_type q.pat_env::rest
    | _ -> fatal_error "Parmatch.do_set_args (lazy)"
    end
| {pat_desc = Tpat_array omegas} ->
    let args,rest = read_args omegas r in
    make_pat
      (Tpat_array args) q.pat_type q.pat_env::
    rest
| {pat_desc=Tpat_constant _|Tpat_any} ->
    q::r (* case any is used in matching.ml *)
| _ -> fatal_error "Parmatch.set_args"
   *)

let do_set_args erase_mutable q r = match q.sp_head with
| Stuple arity ->
    let args,rest = read_args arity r in
    make_pat (Tpat_tuple args) q.sp_type q.sp_env::rest
| Srecord { closed; fields; all_labels } ->
    let args,rest = read_args (IntSet.cardinal fields) r in
    make_pat
      (Tpat_record
         (List.map2 (fun lbl_pos arg ->
           let lbl = all_labels.(lbl_pos) in
           (* NB. we're losing elements of the path here *)
           let lid = mknoloc (Longident.Lident lbl.lbl_name) in
           if
             erase_mutable &&
             (match lbl.lbl_mut with
             | Mutable -> true | Immutable -> false)
           then
             lid, lbl, omega
           else
             lid, lbl, arg)
            (IntSet.elements fields) args, closed))
      q.sp_type q.sp_env::
    rest
| Sconstruct c ->
    let args,rest = read_args c.cstr_arity r in
    make_pat
      (* NB. once again, we're losing elements of the path here *)
      (Tpat_construct (mknoloc (Longident.Lident c.cstr_name), c,args))
      q.sp_type q.sp_env::
    rest
| Svariant { name = l; has_argument; row } ->
    let arg, rest =
      match has_argument, r with
        true, a::r -> Some a, r
      | false, r -> None, r
      | _ -> assert false
    in
    make_pat
      (Tpat_variant (l, arg, row)) q.sp_type q.sp_env::
    rest
| Slazy ->
    begin match r with
      arg::rest ->
        make_pat (Tpat_lazy arg) q.sp_type q.sp_env::rest
    | _ -> fatal_error "Parmatch.do_set_args (lazy)"
    end
| Sarray len ->
    let args,rest = read_args len r in
    make_pat
      (Tpat_array args) q.sp_type q.sp_env::
    rest
| Sconstant c ->
    make_pat (Tpat_constant c) q.sp_type q.sp_env :: r
| Sany ->
    make_pat Tpat_any q.sp_type q.sp_env::r (* case any is used in matching.ml *)

let set_args q r = do_set_args false q r
and set_args_erase_mutable q r = do_set_args true q r

(* Given a matrix of non-empty rows
   p1 :: r1...
   p2 :: r2...
   p3 :: r3...

   Simplify the first column [p1 p2 p3] by splitting all or-patterns.
   The result is a list of couples
     (simple pattern, rest of row)
   where a "simple pattern" starts with either the catch-all pattern omega (_)
   or a head constructor.
   pushed as a new varset.

   For example,
     x :: r1
     (Some _) as y :: r2
     (None as x) as y :: r3
     (Some x | (None as x)) :: r4
   becomes
     (_, r1)
     (Some _, r2)
     (None, r3)
     (Some x, r4)
     (None, r4)
 *)
let rec simplify_first_col = function
  | [] -> []
  | [] :: _ -> assert false (* the rows are non-empty! *)
  | (p::ps) :: rows -> simplify_head_pat p ps (simplify_first_col rows)

and simplify_head_pat p ps k =
  match p.pat_desc with
  | Tpat_alias (p,_,_) -> simplify_head_pat p ps k
  | Tpat_var (_,_) -> (omega, ps) :: k
  | Tpat_or (p1,p2,_) -> simplify_head_pat p1 ps (simplify_head_pat p2 ps k)
  | _ -> (p, ps) :: k


(* The default matrix was introduced in section 3.1 of
   http://moscova.inria.fr/~maranget/papers/warn/warn.pdf .

   Here we make the assumption that the first column of the matrix was
   previously simplified to contain only _ or a head constructor.
*)
let build_default_matrix pss =
  let rec filter_rec = function
    | [] -> []
    | row :: rows ->
      match row with
      | ({pat_desc=(Tpat_var _|Tpat_alias _|Tpat_or _)},_) -> assert false
      | ({pat_desc = Tpat_any}, qs) ->
        qs :: filter_rec rows
      | _ -> filter_rec rows
  in
  filter_rec pss

(* Builds the specialized matrix of [pss] according to pattern [q].
   See section 3.1 of http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

   NOTES:
   - expects [pss] to be a "simplified matrix"
   - [q] was produced by [discr_pat]
*)
let build_specialized_submatrix ~extend_row q pss =
  let rec filter_rec = function
    | ({pat_desc = (Tpat_alias _ | Tpat_or _ | Tpat_var _) }, _) :: _ ->
        assert false
    | (p, ps) :: pss ->
        if simple_match q.sp_head p
        then extend_row (simple_match_args q.sp_head p) ps :: filter_rec pss
        else filter_rec pss
    | _ -> [] in
  filter_rec pss

(* Consider a pattern matrix whose first column has been simplified
   to contain only _ or a head constructor
     | p1, r1...
     | p2, r2...
     | p3, r3...
     | ...

   We split this matrix into a list of /specialized/ sub-matrices [1], one for
   each head constructor appearing in the first column. For each row whose
   first column starts with a head constructor, remove this head
   column, prepend one column for each argument of the constructor,
   and add the resulting row in the sub-matrix corresponding to this
   head constructor.

   Rows whose left column is omega (the Any pattern _) may match any
   head constructor, so they are added to all sub-matrices.

   In the case where all the rows in the matrix have an omega on their first
   column, then there is only one "specialized" sub-matrix, formed of all these
   omega rows. The [return_omega_group] parameter is used to control whether
   this lone sub-matrix should be returned or not.

   [1]: specialized sub-matrices are introduced in section 3.1 of
   http://moscova.inria.fr/~maranget/papers/warn/warn.pdf .

*)
let build_specialized_submatrices ~return_omega_group ~extend_row q rows =
  let extend_group discr p r rs =
    let r = extend_row (simple_match_args discr.sp_head p) r in
    (discr, r :: rs) in

  (* insert a row of head [p] and rest [r] into the right group *)
  let rec insert_constr p r env = match env with
  | [] ->
      (* if no group matched this row, it has a head constructor that
         was never seen before; add a new sub-matrix for this head *)
      [extend_group (normalize_pat p) p r []]
  | (q0,rs) as bd::env ->
      if simple_match q0.sp_head p
      then extend_group q0 p r rs :: env
      else bd :: insert_constr p r env in

  (* insert a row of head omega into all groups *)
  let insert_omega r env =
    List.map (fun (q0,rs) -> extend_group q0 omega r rs) env in

  let rec form_groups constr_groups omega_tails = function
    | [] -> (constr_groups, omega_tails)
    | ({pat_desc=(Tpat_var _|Tpat_alias _|Tpat_or _)},_)::_ -> assert false
    | ({pat_desc=Tpat_any}, tail) :: rest ->
       (* note that calling insert_omega here would be wrong
          as some groups may not have been formed yet, if the
          first row with this head pattern comes after in the list *)
       form_groups constr_groups (tail :: omega_tails) rest
    | (p,r) :: rest ->
       form_groups (insert_constr p r constr_groups) omega_tails rest in

  let constr_groups, omega_tails =
    let initial_constr_group =
      match q.sp_head with
      | Srecord(_) | Stuple(_) | Slazy ->
        (* [q] comes from [discr_pat], and in this case subsumes any of the
           patterns we could find on the first column of [rows]. So it is better
           to use it for our initial environment than any of the normalized
           pattern we might obtain from the first column. *)
        [q,[]]
      | _ -> []
    in
    form_groups initial_constr_group [] rows
  in
  if constr_groups = [] then (
    (* no head constructors: the omega tails form a single submatrix *)
    if return_omega_group
    then [q, omega_tails]
    else []
  ) else (
    (* insert omega rows in all groups *)
    List.fold_right insert_omega omega_tails constr_groups
  )

(* Variant related functions *)

let set_last a =
  let rec loop = function
    | [] -> assert false
    | [_] -> [a]
    | x::l -> x :: loop l
  in
  function
  | (_, []) -> (a, [])
  | (first, row) -> (first, loop row)

(* mark constructor lines for failure when they are incomplete

   Precondition: the input matrix has been simplified so that its
   first column only contains _ or head constructors. *)
let mark_partial =
  List.map (function
    | ({pat_desc=(Tpat_var _|Tpat_alias _|Tpat_or _)},_) -> assert false
    | ({pat_desc = Tpat_any }, _) as ps -> ps
    | ps -> set_last zero ps
  )

let close_variant env row =
  let row = Btype.row_repr row in
  let nm =
    List.fold_left
      (fun nm (_tag,f) ->
        match Btype.row_field_repr f with
        | Reither(_, _, false, e) ->
            (* m=false means that this tag is not explicitly matched *)
            Btype.set_row_field e Rabsent;
            None
        | Rabsent | Reither (_, _, true, _) | Rpresent _ -> nm)
      row.row_name row.row_fields in
  if not row.row_closed || nm != row.row_name then begin
    (* this unification cannot fail *)
    Ctype.unify env row.row_more
      (Btype.newgenty
         (Tvariant {row with row_fields = []; row_more = Btype.newgenvar();
                    row_closed = true; row_name = nm}))
  end

(*
  Check whether the first column of env makes up a complete signature or
  not.
*)

let full_match closing env =
  match env with
  | [] -> assert false
  | (sp, _) :: _ ->
    match sp.sp_head with
    | Sconstruct cd ->
      if cd.cstr_consts < 0 then false (* extensions *)
      else List.length env = cd.cstr_consts + cd.cstr_nonconsts
    | Svariant _ ->
      let fields =
        List.map (function
          | ({ sp_head = Svariant { name = tag; _ }}, _) -> tag
          | _ ->
            (* This is safe because [env] is the set of specialized submatrices.
               So [sp_head] is one of the head constructors used to "specialise"
               the initial matrix.
               Therefore it can be [Sany], and because of typing (of the code
               being examined) cannot be anything other than [Svariant] either.
            *)
            assert false
        )
          env
      in
      (* We are not using [_row_desc] as it might not have been set yet (it is
         set after the call to [pressure_variants] for example). *)
      let row = row_of_type sp.sp_env sp.sp_type in
      if closing && not (Btype.row_fixed row) then
        (* closing=true, we are considering the variant as closed *)
        List.for_all
          (fun (tag,f) ->
             match Btype.row_field_repr f with
               Rabsent | Reither(_, _, false, _) -> true
             | Reither (_, _, true, _)
             (* m=true, do not discard matched tags, rather warn *)
             | Rpresent _ -> List.mem tag fields)
          row.row_fields
      else
        row.row_closed &&
        List.for_all
          (fun (tag,f) ->
             Btype.row_field_repr f = Rabsent || List.mem tag fields)
          row.row_fields
    | Sconstant(Const_char _) ->
      List.length env = 256
    | Sconstant(_) -> false
    | Stuple(_) -> true
    | Srecord(_) -> true
    | Sarray(_) -> false
    | Slazy -> true
    | Sany -> assert false

(* Written as a non-fragile matching, PR7451 originated from a fragile matching below. *)
let should_extend ext env = match ext with
| None -> false
| Some ext -> begin match env with
  | [] -> assert false
  | (sp,_)::_ ->
      begin match sp.sp_head with
      | Sconstruct {cstr_tag=(Cstr_constant _|Cstr_block _|Cstr_unboxed)} ->
          (match get_constructor_type_path p.pat_type p.pat_env with
           | Ok path -> Path.same path ext
           | Inconsistent_environment ->
             (* returning [true] here could result in more computations being
                done to check exhaustivity. Which is clearly not necessary
                since the code doesn't typecheck anyway. *)
             false)
      | Sconstruct {cstr_tag=(Cstr_extension _)} -> false
      | Sconstant _|Stuple _|Svariant _
      | Srecord  _|Sarray _ | Slazy
        -> false
      | Sany
        -> assert false
      end
end

module ConstructorTagHashtbl = Hashtbl.Make(
  struct
    type t = Types.constructor_tag
    let hash = Hashtbl.hash
    let equal = Types.equal_tag
  end
)

(* complement constructor tags *)
let complete_tags nconsts nconstrs tags =
  let seen_const = Array.make nconsts false
  and seen_constr = Array.make nconstrs false in
  List.iter
    (function
      | Cstr_constant i -> seen_const.(i) <- true
      | Cstr_block i -> seen_constr.(i) <- true
      | _  -> assert false)
    tags ;
  let r = ConstructorTagHashtbl.create (nconsts+nconstrs) in
  for i = 0 to nconsts-1 do
    if not seen_const.(i) then
      ConstructorTagHashtbl.add r (Cstr_constant i) ()
  done ;
  for i = 0 to nconstrs-1 do
    if not seen_constr.(i) then
      ConstructorTagHashtbl.add r (Cstr_block i) ()
  done ;
  r

(* build a pattern from a constructor description *)
let pat_of_constr ex_pat cstr =
  { pat_desc = Tpat_construct (mknoloc (Longident.Lident "?pat_of_constr?"),
                               cstr, omegas cstr.cstr_arity)
  ; pat_env = ex_pat.sp_env
  ; pat_type = ex_pat.sp_type
  ; pat_extra = []
  ; pat_attributes = []
  ; pat_loc = Location.none
  }

let orify x y = make_pat (Tpat_or (x, y, None)) x.pat_type x.pat_env

let rec orify_many = function
| [] -> assert false
| [x] -> x
| x :: xs -> orify x (orify_many xs)

(* build an or-pattern from a constructor list *)
let pat_of_constrs ex_pat cstrs =
  if cstrs = [] then raise Empty else
  orify_many (List.map (pat_of_constr ex_pat) cstrs)

let pats_of_type ?(always=false) env ty =
  let ty' = Ctype.expand_head env ty in
  match ty'.desc with
  | Tconstr (path, _, _) ->
      begin try match (Env.find_type path env).type_kind with
      | Type_variant cl when always || List.length cl = 1 ||
        List.for_all (fun cd -> cd.Types.cd_res <> None) cl ->
          let cstrs = fst (Env.find_type_descrs path env) in
          List.map (pat_of_constr { sp_head = Sany; sp_type = ty; sp_env = env })
            cstrs
      | Type_record _ ->
          let labels = snd (Env.find_type_descrs path env) in
          let fields =
            List.map (fun ld ->
              mknoloc (Longident.Lident "?pat_of_label?"), ld, omega)
              labels
          in
          [make_pat (Tpat_record (fields, Closed)) ty env]
      | _ -> [omega]
      with Not_found -> [omega]
      end
  | Ttuple tl ->
      [make_pat (Tpat_tuple (omegas (List.length tl))) ty env]
  | _ -> [omega]

let rec get_variant_constructors env ty =
  match (Ctype.repr ty).desc with
  | Tconstr (path,_,_) -> begin
      try match Env.find_type path env with
      | {type_kind=Type_variant _} ->
          fst (Env.find_type_descrs path env)
      | {type_manifest = Some _} ->
          get_variant_constructors env
            (Ctype.expand_head_once env (clean_copy ty))
      | _ -> fatal_error "Parmatch.get_variant_constructors"
      with Not_found ->
        fatal_error "Parmatch.get_variant_constructors"
    end
  | _ -> fatal_error "Parmatch.get_variant_constructors"

(* Sends back a pattern that complements constructor tags all_tag *)
let complete_constrs p all_tags =
  let c =
    match p.sp_head with Sconstruct c -> c | _ -> assert false in
  let not_tags = complete_tags c.cstr_consts c.cstr_nonconsts all_tags in
  let constrs = get_variant_constructors p.sp_env c.cstr_res in
  let others =
    List.filter
      (fun cnstr -> ConstructorTagHashtbl.mem not_tags cnstr.cstr_tag)
      constrs in
  let const, nonconst =
    List.partition (fun cnstr -> cnstr.cstr_arity = 0) others in
  const @ nonconst

let build_other_constrs env p =
  match p.sp_head with
  | Sconstruct {cstr_tag=Cstr_constant _|Cstr_block _; _} ->
      let get_tag = function
        | Sconstruct cd -> cd.cstr_tag
        | _ -> fatal_error "Parmatch.get_tag" in
      let all_tags =  List.map (fun (p,_) -> get_tag p.sp_head) env in
      pat_of_constrs p (complete_constrs p all_tags)
  | _ -> extra_pat

(* Auxiliary for build_other *)

let build_other_constant proj make first next sp env =
  let all = List.map (fun (sp, _) -> proj sp.sp_head) env in
  let rec try_const i =
    if List.mem i all
    then try_const (next i)
    else make_pat (make i) sp.sp_type sp.sp_env
  in try_const first

(*
  Builds a pattern that is incompatible with all patterns in
  the first column of env

   N.B. [env] is the list of specialized submatrices along with the head
   constructor used to specialize them.
   This functions contains various matching on the first column of which
   [assert false] all but one head constructor.
   In all these cases we already know the first column of the first row of
   [env], from which we can deduce the safety of the various [assert false]s,
   indeed:
   - [Sany] cannot be present on the first column (see the documentation of
   [build_specialized_submatrices] to understand why that is true).
   - all the simple patterns of the first column must be of the same kind
   (because otherwise the matching being examined wouldn't typecheck).
*)
let build_other ext env = match env with
| [] -> omega
| (sp, _) :: _ ->
    match sp.sp_head with
    | Sconstruct {cstr_tag=Cstr_extension _} ->
        (* let c = {c with cstr_name = "*extension*"} in *) (* PR#7330 *)
        make_pat (Tpat_var (Ident.create "*extension*",
                            mknoloc "*extension*")) Ctype.none Env.empty
    | Sconstruct _ ->
        begin match ext with
        | Some ext ->
            (match get_constructor_type_path p.pat_type p.pat_env with
             | Ok path when Path.same ext path -> extra_pat
             | _ -> build_other_constrs env p)
        | _ ->
            build_other_constrs env sp
        end
    | Svariant { row = r; _ } ->
        let tags =
          List.map
            (function ({ sp_head = Svariant { name = tag; _ }}, _) -> tag
                    | _ -> assert false)
            env
        in
        let row = row_of_type sp.sp_env sp.sp_type in
        let make_other_pat tag const =
          let arg = if const then None else Some omega in
          make_pat (Tpat_variant(tag, arg, r)) sp.sp_type sp.sp_env in
        begin match
          List.fold_left
            (fun others (tag,f) ->
               if List.mem tag tags then others else
                 match Btype.row_field_repr f with
                   Rabsent (* | Reither _ *) -> others
                 (* This one is called after erasing pattern info *)
                 | Reither (c, _, _, _) -> make_other_pat tag c :: others
                 | Rpresent arg -> make_other_pat tag (arg = None) :: others)
            [] row.row_fields
        with
          [] ->
          make_other_pat "AnyExtraTag" true
        | pat::other_pats ->
          List.fold_left
            (fun p_res pat ->
               make_pat (Tpat_or (pat, p_res, None)) sp.sp_type sp.sp_env)
            pat other_pats
        end
    | Sconstant(Const_char _) ->
        let all_chars =
          List.map
            (fun (sp,_) -> match sp.sp_head with
            | Sconstant (Const_char c) -> c
            | _ -> assert false)
            env in

        let rec find_other i imax =
          if i > imax then raise Not_found
          else
            let ci = Char.chr i in
            if List.mem ci all_chars then
              find_other (i+1) imax
            else
              make_pat (Tpat_constant (Const_char ci)) sp.sp_type sp.sp_env in
        let rec try_chars = function
          | [] -> omega
          | (c1,c2) :: rest ->
              try
                find_other (Char.code c1) (Char.code c2)
              with
              | Not_found -> try_chars rest in

        try_chars
          [ 'a', 'z' ; 'A', 'Z' ; '0', '9' ;
            ' ', '~' ; Char.chr 0 , Char.chr 255]

    | (Sconstant (Const_int _)) ->
        build_other_constant
          (function Sconstant(Const_int i) -> i | _ -> assert false)
          (function i -> Tpat_constant(Const_int i))
          0 succ sp env
    | (Sconstant (Const_int32 _)) ->
        build_other_constant
          (function Sconstant(Const_int32 i) -> i | _ -> assert false)
          (function i -> Tpat_constant(Const_int32 i))
          0l Int32.succ sp env
    | (Sconstant (Const_int64 _)) ->
        build_other_constant
          (function Sconstant(Const_int64 i) -> i | _ -> assert false)
          (function i -> Tpat_constant(Const_int64 i))
          0L Int64.succ sp env
    | (Sconstant (Const_nativeint _)) ->
        build_other_constant
          (function Sconstant(Const_nativeint i) -> i | _ -> assert false)
          (function i -> Tpat_constant(Const_nativeint i))
          0n Nativeint.succ sp env
    | (Sconstant (Const_string _)) ->
        build_other_constant
          (function Sconstant(Const_string (s, _)) -> String.length s
                  | _ -> assert false)
          (function i -> Tpat_constant(Const_string(String.make i '*', None)))
          0 succ sp env
    | (Sconstant (Const_float _)) ->
        build_other_constant
          (function Sconstant(Const_float f) -> float_of_string f
                  | _ -> assert false)
          (function f -> Tpat_constant(Const_float (string_of_float f)))
          0.0 (fun f -> f +. 1.0) sp env
  | Sarray _ ->
      let all_lengths =
        List.map
          (fun (sp,_) -> match sp.sp_head with
          | Sarray arity -> arity
          | _ -> assert false)
          env in
      let rec try_arrays l =
        if List.mem l all_lengths then try_arrays (l+1)
        else
          make_pat
            (Tpat_array (omegas l))
            sp.sp_type sp.sp_env in
      try_arrays 0
  | _ -> omega


let rec has_instance p = match p.pat_desc with
  | Tpat_variant (l,_,r) when is_absent l r -> false
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_,None,_) -> true
  | Tpat_alias (p,_,_) | Tpat_variant (_,Some p,_) -> has_instance p
  | Tpat_or (p1,p2,_) -> has_instance p1 || has_instance p2
  | Tpat_construct (_,_,ps) | Tpat_tuple ps | Tpat_array ps ->
      has_instances ps
  | Tpat_record (lps,_) -> has_instances (List.map (fun (_,_,x) -> x) lps)
  | Tpat_lazy p
    -> has_instance p


and has_instances = function
  | [] -> true
  | q::rem -> has_instance q && has_instances rem

let rec collect_witnesses ~only_care_about_existence pred = function
  | [] -> []
  | x :: xs ->
    let witnesses = pred x in
    match witnesses with
    | _ :: _ when only_care_about_existence -> witnesses
    | _ ->
      (* This is not tail-rec but neither was [List.flatten (List.map ...)]
         which was used previously. *)
      witnesses @ collect_witnesses ~only_care_about_existence pred xs

let rec list_satisfying_vectors ~only_care_about_existence pss qs =
  let list_satisfying_vectors = list_satisfying_vectors ~only_care_about_existence in
  match pss with
  | [] -> if has_instances qs then [qs] else []
  | _  ->
      match qs with
      | [] -> []
      | {pat_desc = Tpat_or(q1,q2,_)}::qs ->
          let witnesses = list_satisfying_vectors pss (q1::qs) in
          begin match witnesses with
          | _ :: _ when only_care_about_existence -> witnesses
          | _ -> witnesses @ list_satisfying_vectors pss (q2::qs)
          end
      | {pat_desc = Tpat_alias(q,_,_)}::qs ->
          list_satisfying_vectors pss (q::qs)
      | {pat_desc = (Tpat_any | Tpat_var(_))}::qs ->
          let simplified = simplify_first_col pss in
          let q0 = discr_pat omega simplified in
          let wild p =
            List.map (fun qs -> p::qs)
              (list_satisfying_vectors (build_default_matrix simplified) qs)
          in
          begin match
            build_specialized_submatrices ~return_omega_group:false ~extend_row:(@)
              q0 simplified
          with
            (* first column of pss is made of variables only *)
          | [] ->
              wild omega
          | (p,_)::_ as constrs  ->
              let for_constrs () =
                collect_witnesses ~only_care_about_existence (fun (p,pss) ->
                  if is_absent_pat p then
                    []
                  else
                    let witnesses =
                      list_satisfying_vectors pss
                        (simple_match_args p.sp_head omega @ qs)
                    in
                    if only_care_about_existence then
                      witnesses
                    else
                      List.map (set_args p) witnesses
                ) constrs
              in
              if full_match false constrs then for_constrs () else
              match p.sp_head with
                Sconstruct _ when not only_care_about_existence ->
                  (* activate this code for checking non-gadt constructors *)
                  wild (build_other_constrs constrs p) @ for_constrs ()
              | _ ->
                  wild omega
          end
      | {pat_desc=Tpat_variant (l,_,r)}::_ when is_absent l r -> []
      | q::qs ->
          let simplified = simplify_first_col pss in
          let q0 = discr_pat q simplified in
          let sub_witnesses =
            list_satisfying_vectors
              (build_specialized_submatrix ~extend_row:(@) q0 simplified)
              (simple_match_args q0.sp_head q @ qs)
          in
          if only_care_about_existence then
            sub_witnesses
          else
            List.map (set_args q0) sub_witnesses

(*
  Core function :
  Is the last row of pattern matrix pss + qs satisfiable ?
  That is :
    Does there exists at least one value vector, es such that :
     1- for all ps in pss, ps # es (ps and es are not compatible)
     2- qs <= es                   (es matches qs)
*)
let satisfiable pss qs =
  match list_satisfying_vectors ~only_care_about_existence:true pss qs with
  | [] -> false
  | _ -> true

(* While [satisfiable] only checks whether the last row of [pss + qs] is
   satisfiable, this function returns the (possibly empty) list of vectors [es]
   which verify:
     1- for all ps in pss, ps # es (ps and es are not compatible)
     2- qs <= es                   (es matches qs)

   This is done to enable GADT handling *)
let satisfiables pss qs =
  list_satisfying_vectors ~only_care_about_existence:false pss qs

(******************************************)
(* Look for a row that matches some value *)
(******************************************)

(*
  Useful for seeing if the example of
  non-matched value can indeed be matched
  (by a guarded clause)
*)

let rec do_match pss qs = match qs with
| [] ->
    begin match pss  with
    | []::_ -> true
    | _ -> false
    end
| q::qs -> match q with
  | {pat_desc = Tpat_or (q1,q2,_)} ->
      do_match pss (q1::qs) || do_match pss (q2::qs)
  | {pat_desc = Tpat_any} ->
      let rec remove_first_column = function
        | (_::ps)::rem -> ps::remove_first_column rem
        | _ -> []
      in
      do_match (remove_first_column pss) qs
  | _ ->
      let q0 = normalize_pat q in
      do_match
        (build_specialized_submatrix ~extend_row:(@) q0
           (simplify_first_col pss))
        (simple_match_args q0.sp_head q @ qs)


type 'a exhaust_result =
  | No_matching_value
  | Witnesses of 'a list

let rappend r1 r2 =
  match r1, r2 with
  | No_matching_value, _ -> r2
  | _, No_matching_value -> r1
  | Witnesses l1, Witnesses l2 -> Witnesses (l1 @ l2)

let rec try_many  f = function
  | [] -> No_matching_value
  | (p,pss)::rest ->
      rappend (f (p, pss)) (try_many f rest)

(*
let print_pat pat =
  let rec string_of_pat pat =
    match pat.pat_desc with
        Tpat_var _ -> "v"
      | Tpat_any -> "_"
      | Tpat_alias (p, x) -> Printf.sprintf "(%s) as ?"  (string_of_pat p)
      | Tpat_constant n -> "0"
      | Tpat_construct (_, lid, _) ->
        Printf.sprintf "%s" (String.concat "." (Longident.flatten lid.txt))
      | Tpat_lazy p ->
        Printf.sprintf "(lazy %s)" (string_of_pat p)
      | Tpat_or (p1,p2,_) ->
        Printf.sprintf "(%s | %s)" (string_of_pat p1) (string_of_pat p2)
      | Tpat_tuple list ->
        Printf.sprintf "(%s)" (String.concat "," (List.map string_of_pat list))
      | Tpat_variant (_, _, _) -> "variant"
      | Tpat_record (_, _) -> "record"
      | Tpat_array _ -> "array"
  in
  Printf.fprintf stderr "PAT[%s]\n%!" (string_of_pat pat)
*)

(*
  Now another satisfiable function that additionally
  supplies an example of a matching value.

  This function should be called for exhaustiveness check only.
*)
let rec exhaust (ext:Path.t option) pss n = match pss with
| []    ->  Witnesses [omegas n]
| []::_ ->  No_matching_value
| pss   ->
    let simplified = simplify_first_col pss in
    let q0 = discr_pat omega simplified in
    begin match
      build_specialized_submatrices ~return_omega_group:false ~extend_row:(@)
        q0 simplified
    with
          (* first column of pss is made of variables only *)
    | [] ->
        begin match exhaust ext (build_default_matrix simplified) (n-1) with
        | Witnesses r ->
            let q0 = pat_of_sp q0 in
            Witnesses (List.map (fun row -> q0::row) r)
        | r -> r
      end
    | constrs ->
        let try_non_omega (p,pss) =
          if is_absent_pat p then
            No_matching_value
          else
            match
              exhaust
                ext pss (List.length (simple_match_args p.sp_head omega) + n - 1)
            with
            | Witnesses r -> Witnesses (List.map (fun row ->  (set_args p row)) r)
            | r       -> r in
        let before = try_many try_non_omega constrs in
        if
          full_match false constrs && not (should_extend ext constrs)
        then
          before
        else
          (* as [build_default_matrix pss] is included in [pss] one can avoid
             recursive calls on specialized matrices.
             Essentially:
             - [build_default_matrix pss] exhaustive => [pss] exhaustive
             - [build_default_matrix pss] non-exhastive => we have a
             non-filtered value *)
          let r =  exhaust ext (build_default_matrix simplified) (n-1) in
          match r with
          | No_matching_value -> before
          | Witnesses r ->
              try
                let p = build_other ext constrs in
                let dug = List.map (fun tail -> p :: tail) r in
                match before with
                | No_matching_value -> Witnesses dug
                | Witnesses x -> Witnesses (x @ dug)
              with
      (* cannot occur, since constructors don't make a full signature *)
              | Empty -> fatal_error "Parmatch.exhaust"
    end

let exhaust ext pss n =
  let ret = exhaust ext pss n in
  match ret with
    No_matching_value -> No_matching_value
  | Witnesses lst ->
      let singletons =
        List.map
          (function
              [x] -> x
            | _ -> assert false)
          lst
      in
      Witnesses [orify_many singletons]

(*
   Another exhaustiveness check, enforcing variant typing.
   Note that it does not check exact exhaustiveness, but whether a
   matching could be made exhaustive by closing all variant types.
   When this is true of all other columns, the current column is left
   open (even if it means that the whole matching is not exhaustive as
   a result).
   When this is false for the matrix minus the current column, and the
   current column is composed of variant tags, we close the variant
   (even if it doesn't help in making the matching exhaustive).
*)

let rec pressure_variants tdefs = function
  | []    -> false
  | []::_ -> true
  | pss   ->
      let simplified = simplify_first_col pss in
      let q0 = discr_pat omega simplified in
      begin match
        build_specialized_submatrices ~return_omega_group:false ~extend_row:(@)
          q0 simplified
      with
        [] -> pressure_variants tdefs (build_default_matrix simplified)
      | constrs ->
          let rec try_non_omega = function
              (_p,pss) :: rem ->
                let ok = pressure_variants tdefs pss in
                (* The order below matters : we want [pressure_variants] to be
                   called on all the specialized submatrices because we might
                   close some variant in any of them regardless of whether [ok]
                   is true for [pss] or not *)
                try_non_omega rem && ok
            | [] -> true
          in
          if full_match (tdefs=None) constrs then
            try_non_omega constrs
          else if tdefs = None then
            pressure_variants None (build_default_matrix simplified)
          else
            let full = full_match true constrs in
            let ok =
              if full then try_non_omega constrs
              else try_non_omega
                     (build_specialized_submatrices ~return_omega_group:false
                        ~extend_row:(@) q0
                        (mark_partial simplified))
            in
            begin match constrs, tdefs with
              ({ sp_head = Svariant _ } as sp,_):: _, Some env ->
                let row = row_of_type sp.sp_env sp.sp_type in
                if Btype.row_fixed row
                || pressure_variants None (build_default_matrix simplified) then ()
                else close_variant env row
            | _ -> ()
            end;
            ok
      end


(* Yet another satisfiable fonction *)

(*
   This time every_satisfiable pss qs checks the
   utility of every expansion of qs.
   Expansion means expansion of or-patterns inside qs
*)

type answer =
  | Used                                (* Useful pattern *)
  | Unused                              (* Useless pattern *)
  | Upartial of Typedtree.pattern list  (* Mixed, with list of useless ones *)



(* this row type enable column processing inside the matrix
    - left  ->  elements not to be processed,
    - right ->  elements to be processed
*)
type row = {no_ors : pattern list ; ors : pattern list ; active : pattern list}


(*
let pretty_row {ors=ors ; no_ors=no_ors; active=active} =
  pretty_line ors ; prerr_string " *" ;
  pretty_line no_ors ; prerr_string " *" ;
  pretty_line active

let pretty_rows rs =
  prerr_endline "begin matrix" ;
  List.iter
    (fun r ->
      pretty_row r ;
      prerr_endline "")
    rs ;
  prerr_endline "end matrix"
*)

(* Initial build *)
let make_row ps = {ors=[] ; no_ors=[]; active=ps}

let make_rows pss = List.map make_row pss


(* Useful to detect and expand  or pats inside as pats *)
let rec unalias p = match p.pat_desc with
| Tpat_alias (p,_,_) -> unalias p
| _ -> p


let is_var p = match (unalias p).pat_desc with
| Tpat_any|Tpat_var _ -> true
| _                   -> false

let is_var_column rs =
  List.for_all
    (fun r -> match r.active with
    | p::_ -> is_var p
    | []   -> assert false)
    rs

(* Standard or-args for left-to-right matching *)
let rec or_args p = match p.pat_desc with
| Tpat_or (p1,p2,_) -> p1,p2
| Tpat_alias (p,_,_)  -> or_args p
| _                 -> assert false

(* Just remove current column *)
let remove r = match r.active with
| _::rem -> {r with active=rem}
| []     -> assert false

let remove_column rs = List.map remove rs

(* Current column has been processed *)
let push_no_or r = match r.active with
| p::rem -> { r with no_ors = p::r.no_ors ; active=rem}
| [] -> assert false

let push_or r = match r.active with
| p::rem -> { r with ors = p::r.ors ; active=rem}
| [] -> assert false

let push_or_column rs = List.map push_or rs
and push_no_or_column rs = List.map push_no_or rs

(* Those are adaptations of the previous homonymous functions that
   work on the current column, instead of the first column
*)
let rec simplify_first_col = function
  | [] -> []
  | row :: rows ->
    match row.active with
    | [] -> assert false (* the rows are non-empty! *)
    | p :: ps ->
      simplify_head_pat p { row with active = ps } (simplify_first_col rows)

and simplify_head_pat p ps k =
  match p.pat_desc with
  | Tpat_alias (p,_,_) -> simplify_head_pat p ps k
  | Tpat_var (_,_) -> (omega, ps) :: k
  | Tpat_or (p1,p2,_) -> simplify_head_pat p1 ps (simplify_head_pat p2 ps k)
  | _ -> (p, ps) :: k


(* Back to normal matrices *)
let make_vector r = r.no_ors

let make_matrix rs = List.map make_vector rs


(* Standard union on answers *)
let union_res r1 r2 = match r1, r2 with
| (Unused,_)
| (_, Unused) -> Unused
| Used,_    -> r2
| _, Used   -> r1
| Upartial u1, Upartial u2 -> Upartial (u1@u2)

(* propose or pats for expansion *)
let extract_elements qs =
  let rec do_rec seen = function
    | [] -> []
    | q::rem ->
        {no_ors= List.rev_append seen rem @ qs.no_ors ;
        ors=[] ;
        active = [q]}::
        do_rec (q::seen) rem in
  do_rec [] qs.ors

(* idem for matrices *)
let transpose rs = match rs with
| [] -> assert false
| r::rem ->
    let i = List.map (fun x -> [x]) r in
    List.fold_left
      (List.map2 (fun r x -> x::r))
      i rem

let extract_columns pss qs = match pss with
| [] -> List.map (fun _ -> []) qs.ors
| _  ->
  let rows = List.map extract_elements pss in
  transpose rows

(* Core function
   The idea is to first look for or patterns (recursive case), then
   check or-patterns argument usefulness (terminal case)
*)

let rec every_satisfiables pss qs = match qs.active with
| []     ->
    (* qs is now partitionned,  check usefulness *)
    begin match qs.ors with
    | [] -> (* no or-patterns *)
        if satisfiable (make_matrix pss) (make_vector qs) then
          Used
        else
          Unused
    | _  -> (* n or-patterns -> 2n expansions *)
        List.fold_right2
          (fun pss qs r -> match r with
          | Unused -> Unused
          | _ ->
              match qs.active with
              | [q] ->
                  let q1,q2 = or_args q in
                  let r_loc = every_both pss qs q1 q2 in
                  union_res r r_loc
              | _   -> assert false)
          (extract_columns pss qs) (extract_elements qs)
          Used
    end
| q::rem ->
    let uq = unalias q in
    begin match uq.pat_desc with
    | Tpat_any | Tpat_var _ ->
        if is_var_column pss then
(* forget about ``all-variable''  columns now *)
          every_satisfiables (remove_column pss) (remove qs)
        else
(* otherwise this is direct food for satisfiable *)
          every_satisfiables (push_no_or_column pss) (push_no_or qs)
    | Tpat_or (q1,q2,_) ->
        if
          q1.pat_loc.Location.loc_ghost &&
          q2.pat_loc.Location.loc_ghost
        then
(* syntactically generated or-pats should not be expanded *)
          every_satisfiables (push_no_or_column pss) (push_no_or qs)
        else
(* this is a real or-pattern *)
          every_satisfiables (push_or_column pss) (push_or qs)
    | Tpat_variant (l,_,r) when is_absent l r -> (* Ah Jacques... *)
        Unused
    | _ ->
(* standard case, filter matrix *)
        let simplified = simplify_first_col pss in
        let q0 = discr_pat q simplified in
        every_satisfiables
          (build_specialized_submatrix q0 simplified
             ~extend_row:(fun ps r -> { r with active = ps @ r.active }))
          {qs with active=simple_match_args q0.sp_head q @ rem}
    end

(*
  This function ``every_both'' performs the usefulness check
  of or-pat q1|q2.
  The trick is to call every_satisfied twice with
  current active columns restricted to q1 and q2,
  That way,
  - others orpats in qs.ors will not get expanded.
  - all matching work performed on qs.no_ors is not performed again.
  *)
and every_both pss qs q1 q2 =
  let qs1 = {qs with active=[q1]}
  and qs2 =  {qs with active=[q2]} in
  let r1 = every_satisfiables pss qs1
  and r2 =  every_satisfiables (if compat q1 q2 then qs1::pss else pss) qs2 in
  match r1 with
  | Unused ->
      begin match r2 with
      | Unused -> Unused
      | Used   -> Upartial [q1]
      | Upartial u2 -> Upartial (q1::u2)
      end
  | Used ->
      begin match r2 with
      | Unused -> Upartial [q2]
      | _      -> r2
      end
  | Upartial u1 ->
      begin match r2 with
      | Unused -> Upartial (u1@[q2])
      | Used   -> r1
      | Upartial u2 -> Upartial (u1 @ u2)
      end




(* le_pat p q  means, forall V,  V matches q implies V matches p *)
let rec le_pat p q =
  match (p.pat_desc, q.pat_desc) with
  | (Tpat_var _|Tpat_any),_ -> true
  | Tpat_alias(p,_,_), _ -> le_pat p q
  | _, Tpat_alias(q,_,_) -> le_pat p q
  | Tpat_constant(c1), Tpat_constant(c2) -> const_compare c1 c2 = 0
  | Tpat_construct(_,c1,ps), Tpat_construct(_,c2,qs) ->
      Types.equal_tag c1.cstr_tag c2.cstr_tag && le_pats ps qs
  | Tpat_variant(l1,Some p1,_), Tpat_variant(l2,Some p2,_) ->
      (l1 = l2 && le_pat p1 p2)
  | Tpat_variant(l1,None,_r1), Tpat_variant(l2,None,_) ->
      l1 = l2
  | Tpat_variant(_,_,_), Tpat_variant(_,_,_) -> false
  | Tpat_tuple(ps), Tpat_tuple(qs) -> le_pats ps qs
  | Tpat_lazy p, Tpat_lazy q -> le_pat p q
  | Tpat_record (l1,_), Tpat_record (l2,_) ->
      let ps,qs = records_args l1 l2 in
      le_pats ps qs
  | Tpat_array(ps), Tpat_array(qs) ->
      List.length ps = List.length qs && le_pats ps qs
(* In all other cases, enumeration is performed *)
  | _,_  -> not (satisfiable [[p]] [q])

and le_pats ps qs =
  match ps,qs with
    p::ps, q::qs -> le_pat p q && le_pats ps qs
  | _, _         -> true

let get_mins le ps =
  let rec select_rec r = function
      [] -> r
    | p::ps ->
        if List.exists (fun p0 -> le p0 p) ps
        then select_rec r ps
        else select_rec (p::r) ps in
  select_rec [] (select_rec [] ps)

(*
  lub p q is a pattern that matches all values matched by p and q
  may raise Empty, when p and q are not compatible
*)

let rec lub p q = match p.pat_desc,q.pat_desc with
| Tpat_alias (p,_,_),_      -> lub p q
| _,Tpat_alias (q,_,_)      -> lub p q
| (Tpat_any|Tpat_var _),_ -> q
| _,(Tpat_any|Tpat_var _) -> p
| Tpat_or (p1,p2,_),_     -> orlub p1 p2 q
| _,Tpat_or (q1,q2,_)     -> orlub q1 q2 p (* Thanks god, lub is commutative *)
| Tpat_constant c1, Tpat_constant c2 when const_compare c1 c2 = 0 -> p
| Tpat_tuple ps, Tpat_tuple qs ->
    let rs = lubs ps qs in
    make_pat (Tpat_tuple rs) p.pat_type p.pat_env
| Tpat_lazy p, Tpat_lazy q ->
    let r = lub p q in
    make_pat (Tpat_lazy r) p.pat_type p.pat_env
| Tpat_construct (lid, c1,ps1), Tpat_construct (_,c2,ps2)
      when  Types.equal_tag c1.cstr_tag c2.cstr_tag  ->
        let rs = lubs ps1 ps2 in
        make_pat (Tpat_construct (lid, c1,rs))
          p.pat_type p.pat_env
| Tpat_variant(l1,Some p1,row), Tpat_variant(l2,Some p2,_)
          when  l1=l2 ->
            let r=lub p1 p2 in
            make_pat (Tpat_variant (l1,Some r,row)) p.pat_type p.pat_env
| Tpat_variant (l1,None,_row), Tpat_variant(l2,None,_)
              when l1 = l2 -> p
| Tpat_record (l1,closed),Tpat_record (l2,_) ->
    let rs = record_lubs l1 l2 in
    make_pat (Tpat_record (rs, closed)) p.pat_type p.pat_env
| Tpat_array ps, Tpat_array qs
      when List.length ps = List.length qs ->
        let rs = lubs ps qs in
        make_pat (Tpat_array rs) p.pat_type p.pat_env
| _,_  ->
    raise Empty

and orlub p1 p2 q =
  try
    let r1 = lub p1 q in
    try
      {q with pat_desc=(Tpat_or (r1,lub p2 q,None))}
  with
  | Empty -> r1
with
| Empty -> lub p2 q

and record_lubs l1 l2 =
  let rec lub_rec l1 l2 = match l1,l2 with
  | [],_ -> l2
  | _,[] -> l1
  | (lid1, lbl1,p1)::rem1, (lid2, lbl2,p2)::rem2 ->
      if lbl1.lbl_pos < lbl2.lbl_pos then
        (lid1, lbl1,p1)::lub_rec rem1 l2
      else if lbl2.lbl_pos < lbl1.lbl_pos  then
        (lid2, lbl2,p2)::lub_rec l1 rem2
      else
        (lid1, lbl1,lub p1 p2)::lub_rec rem1 rem2 in
  lub_rec l1 l2

and lubs ps qs = match ps,qs with
| p::ps, q::qs -> lub p q :: lubs ps qs
| _,_ -> []


(******************************)
(* Exported variant closing   *)
(******************************)

(* Apply pressure to variants *)

let pressure_variants tdefs patl =
  let pss = List.map (fun p -> [p;omega]) patl in
  ignore (pressure_variants (Some tdefs) pss)

(*****************************)
(* Utilities for diagnostics *)
(*****************************)

(*
  Build up a working pattern matrix by forgetting
  about guarded patterns
*)

let rec initial_matrix = function
    [] -> []
  | {c_guard=Some _} :: rem -> initial_matrix rem
  | {c_guard=None; c_lhs=p} :: rem -> [p] :: initial_matrix rem

(*
   Build up a working pattern matrix by keeping
   only the patterns which are guarded
*)
let rec initial_only_guarded = function
  | [] -> []
  | { c_guard = None; _} :: rem ->
      initial_only_guarded rem
  | { c_lhs = pat; _ } :: rem ->
      [pat] :: initial_only_guarded rem


(************************)
(* Exhaustiveness check *)
(************************)

(* conversion from Typedtree.pattern to Parsetree.pattern list *)
module Conv = struct
  open Parsetree
  let mkpat desc = Ast_helper.Pat.mk desc

  let name_counter = ref 0
  let fresh name =
    let current = !name_counter in
    name_counter := !name_counter + 1;
    "#$" ^ name ^ string_of_int current

  let conv typed =
    let constrs = Hashtbl.create 7 in
    let labels = Hashtbl.create 7 in
    let rec loop pat =
      match pat.pat_desc with
        Tpat_or (pa,pb,_) ->
          mkpat (Ppat_or (loop pa, loop pb))
      | Tpat_var (_, ({txt="*extension*"} as nm)) -> (* PR#7330 *)
          mkpat (Ppat_var nm)
      | Tpat_any
      | Tpat_var _ ->
          mkpat Ppat_any
      | Tpat_constant c ->
          mkpat (Ppat_constant (Untypeast.constant c))
      | Tpat_alias (p,_,_) -> loop p
      | Tpat_tuple lst ->
          mkpat (Ppat_tuple (List.map loop lst))
      | Tpat_construct (cstr_lid, cstr, lst) ->
          let id = fresh cstr.cstr_name in
          let lid = { cstr_lid with txt = Longident.Lident id } in
          Hashtbl.add constrs id cstr;
          let arg =
            match List.map loop lst with
            | []  -> None
            | [p] -> Some p
            | lst -> Some (mkpat (Ppat_tuple lst))
          in
          mkpat (Ppat_construct(lid, arg))
      | Tpat_variant(label,p_opt,_row_desc) ->
          let arg = Misc.may_map loop p_opt in
          mkpat (Ppat_variant(label, arg))
      | Tpat_record (subpatterns, _closed_flag) ->
          let fields =
            List.map
              (fun (_, lbl, p) ->
                let id = fresh lbl.lbl_name in
                Hashtbl.add labels id lbl;
                (mknoloc (Longident.Lident id), loop p))
              subpatterns
          in
          mkpat (Ppat_record (fields, Open))
      | Tpat_array lst ->
          mkpat (Ppat_array (List.map loop lst))
      | Tpat_lazy p ->
          mkpat (Ppat_lazy (loop p))
    in
    let ps = loop typed in
    (ps, constrs, labels)
end


(* Whether the counter-example contains an extension pattern *)
let contains_extension pat =
  let r = ref false in
  let rec loop = function
      {pat_desc=Tpat_var (_, {txt="*extension*"})} ->
        r := true
    | p -> Typedtree.iter_pattern_desc loop p.pat_desc
  in loop pat; !r

(* Build an untyped or-pattern from its expected type *)
let ppat_of_type env ty =
  match pats_of_type env ty with
    [{pat_desc = Tpat_any}] ->
      (Conv.mkpat Parsetree.Ppat_any, Hashtbl.create 0, Hashtbl.create 0)
  | pats ->
      Conv.conv (orify_many pats)

let check_partial_all v casel =
  (* [v] was produced by [exhaust], so we know it cannot match any of the
     unguarded clause. Therefore we only need to try to match it to one of
     the guarded ones. *)
  let pss = initial_only_guarded casel in
  do_match pss [v]

let do_check_partial ~pred loc casel pss = match pss with
| [] ->
        (*
          This can occur
          - For empty matches generated by ocamlp4 (no warning)
          - when all patterns have guards (then, casel <> [])
          (specific warning)
          Then match MUST be considered non-exhaustive,
          otherwise compilation of PM is broken.
          *)
    begin match casel with
    | [] -> ()
    | _  ->
      if Warnings.is_active Warnings.All_clauses_guarded then
        Location.prerr_warning loc Warnings.All_clauses_guarded
    end ;
    Partial
| ps::_  ->
    begin match exhaust None pss (List.length ps) with
    | No_matching_value -> Total
    | Witnesses [u] ->
        let v =
          let (pattern,constrs,labels) = Conv.conv u in
          let u' = pred constrs labels pattern in
          (* pretty_pat u;
          begin match u' with
            None -> prerr_endline ": impossible"
          | Some _ -> prerr_endline ": possible"
          end; *)
          u'
        in
        begin match v with
          None -> Total
        | Some v ->
            if Warnings.is_active (Warnings.Partial_match "") then begin
              let errmsg =
                try
                  let buf = Buffer.create 16 in
                  let fmt = Format.formatter_of_buffer buf in
                  Printpat.top_pretty fmt v;
                  if check_partial_all v casel then
                    Buffer.add_string buf
                      "\n(However, some guarded clause may match this value.)";
                  if contains_extension v then
                    Buffer.add_string buf
                      "\nMatching over values of extensible variant types \
                         (the *extension* above)\n\
                      must include a wild card pattern in order to be exhaustive."
                  ;
                  Buffer.contents buf
                with _ ->
                  ""
              in
                Location.prerr_warning loc (Warnings.Partial_match errmsg)
            end;
            Partial
        end
    | _ ->
        fatal_error "Parmatch.check_partial"
    end

(*****************)
(* Fragile check *)
(*****************)

(* Collect all data types in a pattern *)

let rec add_path path = function
  | [] -> [path]
  | x::rem as paths ->
      if Path.same path x then paths
      else x::add_path path rem

let extendable_path path =
  not
    (Path.same path Predef.path_bool ||
    Path.same path Predef.path_list ||
    Path.same path Predef.path_unit ||
    Path.same path Predef.path_option)

let rec collect_paths_from_pat r p = match p.pat_desc with
| Tpat_construct(_, {cstr_tag=(Cstr_constant _|Cstr_block _|Cstr_unboxed)},ps)
  ->
    (match get_constructor_type_path p.pat_type p.pat_env with
     | Ok path ->
         List.fold_left
           collect_paths_from_pat
           (if extendable_path path then add_path path r else r)
           ps
     | Inconsistent_environment ->
       (* no need to recurse on the constructor arguments: since we know the
          code won't typecheck anyway, whatever we might compute would be
          useless anyway. *)
       r)
| Tpat_any|Tpat_var _|Tpat_constant _| Tpat_variant (_,None,_) -> r
| Tpat_tuple ps | Tpat_array ps
| Tpat_construct (_, {cstr_tag=Cstr_extension _}, ps)->
    List.fold_left collect_paths_from_pat r ps
| Tpat_record (lps,_) ->
    List.fold_left
      (fun r (_, _, p) -> collect_paths_from_pat r p)
      r lps
| Tpat_variant (_, Some p, _) | Tpat_alias (p,_,_) -> collect_paths_from_pat r p
| Tpat_or (p1,p2,_) ->
    collect_paths_from_pat (collect_paths_from_pat r p1) p2
| Tpat_lazy p
    ->
    collect_paths_from_pat r p


(*
  Actual fragile check
   1. Collect data types in the patterns of the match.
   2. One exhautivity check per datatype, considering that
      the type is extended.
*)

let do_check_fragile loc casel pss =
  let exts =
    List.fold_left
      (fun r c -> collect_paths_from_pat r c.c_lhs)
      [] casel in
  match exts with
  | [] -> ()
  | _ -> match pss with
    | [] -> ()
    | ps::_ ->
        List.iter
          (fun ext ->
            match exhaust (Some ext) pss (List.length ps) with
            | No_matching_value ->
                Location.prerr_warning
                  loc
                  (Warnings.Fragile_match (Path.name ext))
            | Witnesses _ -> ())
          exts

(********************************)
(* Exported unused clause check *)
(********************************)

let check_unused pred casel =
  if Warnings.is_active Warnings.Unused_match
  || List.exists (fun c -> c.c_rhs.exp_desc = Texp_unreachable) casel then
    let rec do_rec pref = function
      | [] -> ()
      | {c_lhs=q; c_guard; c_rhs} :: rem ->
          let qs = [q] in
            begin try
              let pss =
                  get_mins le_pats (List.filter (compats qs) pref) in
              (* First look for redundant or partially redundant patterns *)
              let r = every_satisfiables (make_rows pss) (make_row qs) in
              let refute = (c_rhs.exp_desc = Texp_unreachable) in
              (* Do not warn for unused [pat -> .] *)
              if r = Unused && refute then () else
              let r =
                (* Do not refine if either:
                   - we already know the clause is unused
                   - the clause under consideration is not a refutation clause
                     and either:
                     + there are no other lines
                     + we do not care whether the types prevent this clause to be
                       reached.
                     If the clause under consideration *is* a refutation clause
                     then we do need to check more carefully whether it can be
                     refuted or not.  *)
                let skip =
                  r = Unused || (not refute && pref = []) ||
                  not(refute || Warnings.is_active Warnings.Unreachable_case) in
                if skip then r else
                (* Then look for empty patterns *)
                let sfs = satisfiables pss qs in
                if sfs = [] then Unused else
                let sfs =
                  List.map (function [u] -> u | _ -> assert false) sfs in
                let u = orify_many sfs in
                (*Format.eprintf "%a@." pretty_val u;*)
                let (pattern,constrs,labels) = Conv.conv u in
                let pattern = {pattern with Parsetree.ppat_loc = q.pat_loc} in
                match pred refute constrs labels pattern with
                  None when not refute ->
                    Location.prerr_warning q.pat_loc Warnings.Unreachable_case;
                    Used
                | _ -> r
              in
              match r with
              | Unused ->
                  Location.prerr_warning
                    q.pat_loc Warnings.Unused_match
              | Upartial ps ->
                  List.iter
                    (fun p ->
                      Location.prerr_warning
                        p.pat_loc Warnings.Unused_pat)
                    ps
              | Used -> ()
            with Empty | Not_found -> assert false
            end ;

          if c_guard <> None then
            do_rec pref rem
          else
            do_rec ([q]::pref) rem in

    do_rec [] casel

(*********************************)
(* Exported irrefutability tests *)
(*********************************)

let irrefutable pat = le_pat pat omega

(* An inactive pattern is a pattern whose matching needs only
   trivial computations (tag/equality tests).
   Patterns containing (lazy _) subpatterns are active. *)

let rec inactive pat = match pat with
| Tpat_lazy _ ->
    false
| Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_, None, _) ->
    true
| Tpat_tuple ps | Tpat_construct (_, _, ps) | Tpat_array ps ->
    List.for_all (fun p -> inactive p.pat_desc) ps
| Tpat_alias (p,_,_) | Tpat_variant (_, Some p, _) ->
    inactive p.pat_desc
| Tpat_record (ldps,_) ->
    List.exists (fun (_, _, p) -> inactive p.pat_desc) ldps
| Tpat_or (p,q,_) ->
    inactive p.pat_desc && inactive q.pat_desc

(* A `fluid' pattern is both irrefutable and inactive *)

let fluid pat =  irrefutable pat && inactive pat.pat_desc

(********************************)
(* Exported exhustiveness check *)
(********************************)

(*
   Fragile check is performed when required and
   on exhaustive matches only.
*)

let check_partial pred loc casel =
  let pss = initial_matrix casel in
  let pss = get_mins le_pats pss in
  let total = do_check_partial ~pred loc casel pss in
  if
    total = Total && Warnings.is_active (Warnings.Fragile_match "")
  then begin
    do_check_fragile loc casel pss
  end ;
  total

(*************************************)
(* Ambiguous variable in or-patterns *)
(*************************************)

(* Specification: ambiguous variables in or-patterns.

   The semantics of or-patterns in OCaml is specified with
   a left-to-right bias: a value [v] matches the pattern [p | q] if it
   matches [p] or [q], but if it matches both, the environment
   captured by the match is the environment captured by [p], never the
   one captured by [q].

   While this property is generally well-understood, one specific case
   where users expect a different semantics is when a pattern is
   followed by a when-guard: [| p when g -> e]. Consider for example:

     | ((Const x, _) | (_, Const x)) when is_neutral x -> branch

   The semantics is clear: match the scrutinee against the pattern, if
   it matches, test the guard, and if the guard passes, take the
   branch.

   However, consider the input [(Const a, Const b)], where [a] fails
   the test [is_neutral f], while [b] passes the test [is_neutral
   b]. With the left-to-right semantics, the clause above is *not*
   taken by its input: matching [(Const a, Const b)] against the
   or-pattern succeeds in the left branch, it returns the environment
   [x -> a], and then the guard [is_neutral a] is tested and fails,
   the branch is not taken. Most users, however, intuitively expect
   that any pair that has one side passing the test will take the
   branch. They assume it is equivalent to the following:

     | (Const x, _) when is_neutral x -> branch
     | (_, Const x) when is_neutral x -> branch

   while it is not.

   The code below is dedicated to finding these confusing cases: the
   cases where a guard uses "ambiguous" variables, that are bound to
   different parts of the scrutinees by different sides of
   a or-pattern. In other words, it finds the cases where the
   specified left-to-right semantics is not equivalent to
   a non-deterministic semantics (any branch can be taken) relatively
   to a specific guard.
*)

module IdSet = Set.Make(Ident)

let pattern_vars p = IdSet.of_list (Typedtree.pat_bound_idents p)

(* Row for ambiguous variable search,
   row is the traditional pattern row,
   varsets contain a list of head variable sets (varsets)

   A given varset contains all the variables that appeared at the head
   of a pattern in the row at some point during traversal: they would
   all be bound to the same value at matching time. On the contrary,
   two variables of different varsets appeared at different places in
   the pattern and may be bound to distinct sub-parts of the matched
   value.

   All rows of a (sub)matrix have rows of the same length,
   but also varsets of the same length.
*)

type amb_row = { row : pattern list ; varsets : IdSet.t list; }

(* Given a matrix of non-empty rows
   p1 :: r1...
   p2 :: r2...
   p3 :: r3...

   Simplify the first column [p1 p2 p3] by splitting all or-patterns and
   collecting the head-bound variables (the varset). The result is a list of
   couples
     (simple pattern, rest of row)
   where a "simple pattern" starts with either the catch-all pattern omega (_)
   or a head constructor, and the "rest of the row" has the head-bound variables
   pushed as a new varset.

   For example,
     { row = x :: r1; varsets = s1 }
     { row = (Some _) as y :: r2; varsets  = s2 }
     { row = (None as x) as y :: r3; varsets = s3 }
     { row = (Some x | (None as x)) :: r4 with varsets = s4 }
   becomes
     (_, { row = r1; varsets = s1 ++ {x} })
     (Some _, { row = r2; varsets = s2 ++ {y} })
     (None, { row = r3; varsets = s3 ++ {x, y} })
     (Some x, { row = r4; varsets = s4 })
     (None, { row = r4; varsets = s4 ++ {x} })
 *)
let rec simplify_first_col = function
  | [] -> []
  | { row = [] } :: _ -> assert false
  | { row = p::ps; varsets; }::rem ->
      simplify_head_pat IdSet.empty p ps varsets (simplify_first_col rem)

and simplify_head_pat head_bound_variables p ps varsets k =
  match p.pat_desc with
  | Tpat_alias (p,x,_) ->
    simplify_head_pat (IdSet.add x head_bound_variables) p ps varsets k
  | Tpat_var (x,_) ->
    let rest_of_the_row =
      { row = ps; varsets = IdSet.add x head_bound_variables :: varsets; }
    in
    (omega, rest_of_the_row) :: k
  | Tpat_or (p1,p2,_) ->
    simplify_head_pat head_bound_variables p1 ps varsets
      (simplify_head_pat head_bound_variables p2 ps varsets k)
  | _ ->
    (p, { row = ps; varsets = head_bound_variables :: varsets; }) :: k


let split_rows rows =
  let extend_row columns r =
    { r with row = columns @ r.row } in
  let q0 = discr_pat omega rows in
  build_specialized_submatrices ~return_omega_group:true ~extend_row q0 rows
  |> List.map snd

(* Compute stable bindings *)

let reduce f = function
| [] -> invalid_arg "reduce"
| x::xs -> List.fold_left f x xs

let rec matrix_stable_vars rs = match rs with
| [] -> assert false (* No empty matrix *)
| { row = []; _ } :: _ ->
    (* All rows have the same number of columns;
       if the first row is empty, they all are. *)
    List.iter (fun {row; _} -> assert (row = [])) rs;

    (* A variable is stable in a given varset if, in each row, it
       appears in this varset -- rather than in another position in
       the list of binding sets. We can thus compute the stable
       variables of each varset by pairwise intersection. *)
    let rows_varsets = List.map (fun { varsets; _ } -> varsets) rs in
    let stables_in_varsets = reduce (List.map2 IdSet.inter) rows_varsets in

    (* The stable variables are those stable at any position *)
    List.fold_left IdSet.union IdSet.empty stables_in_varsets
| rs ->
   let submatrices = split_rows (simplify_first_col rs) in
   let submat_stable = List.map matrix_stable_vars submatrices in
   (* a stable variable must be stable in each submatrix;
      if the matrix has at least one row, there is at least one submatrix *)
   reduce IdSet.inter submat_stable

let pattern_stable_vars p = matrix_stable_vars [{varsets = []; row = [p]}]

(* All identifier paths that appear in an expression that occurs
   as a clause right hand side or guard.

  The function is rather complex due to the compilation of
  unpack patterns by introducing code in rhs expressions
  and **guards**.

  For pattern (module M:S)  -> e the code is
  let module M_mod = unpack M .. in e

  Hence M is "free" in e iff M_mod is free in e.

  Not doing so will yield excessive  warning in
  (module (M:S) } ...) when true -> ....
  as M is always present in
  let module M_mod = unpack M .. in true
*)

let all_rhs_idents exp =
  let ids = ref IdSet.empty in
  let module Iterator = TypedtreeIter.MakeIterator(struct
    include TypedtreeIter.DefaultIteratorArgument
    let enter_expression exp = match exp.exp_desc with
      | Texp_ident (path, _lid, _descr) ->
          List.iter
            (fun id -> ids := IdSet.add id !ids)
            (Path.heads path)
      | _ -> ()

(* Very hackish, detect unpack pattern  compilation
   and perfom "indirect check for them" *)
    let is_unpack exp =
      List.exists
        (fun (attr, _) -> attr.txt = "#modulepat") exp.exp_attributes

    let leave_expression exp =
      if is_unpack exp then begin match exp.exp_desc with
      | Texp_letmodule
          (id_mod,_,
           {mod_desc=
            Tmod_unpack ({exp_desc=Texp_ident (Path.Pident id_exp,_,_)},_)},
           _) ->
             assert (IdSet.mem id_exp !ids) ;
             if not (IdSet.mem id_mod !ids) then begin
               ids := IdSet.remove id_exp !ids
             end
      | _ -> assert false
      end
  end) in
  Iterator.iter_expression exp;
  !ids

let check_ambiguous_bindings =
  let open Warnings in
  let warn0 = Ambiguous_pattern [] in
  fun cases ->
    if is_active warn0 then
      List.iter
        (fun case -> match case with
        | { c_guard=None ; _} -> ()
        | { c_lhs=p; c_guard=Some g; _} ->
            let all =
              IdSet.inter (pattern_vars p) (all_rhs_idents g) in
            if not (IdSet.is_empty all) then begin
              let stable = pattern_stable_vars p in
              let ambiguous = IdSet.diff all stable in
              if not (IdSet.is_empty ambiguous) then begin
                let pps = IdSet.elements ambiguous |> List.map Ident.name in
                let warn = Ambiguous_pattern pps in
                Location.prerr_warning p.pat_loc warn
              end
            end)
        cases
