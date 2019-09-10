open Asttypes
open Typedtree
open Types

val omega : pattern
(** aka. "Tpat_any" or "_"  *)

val omegas : int -> pattern list
(** [List.init (fun _ -> omega)] *)

val omega_list : 'a list -> pattern list
(** [List.map (fun _ -> omega)] *)

module Non_empty_row : sig
  type 'a t = 'a * Typedtree.pattern list

  val of_initial : Typedtree.pattern list -> Typedtree.pattern t
  (** 'assert false' on empty rows *)

  val map_first : ('a -> 'b) -> 'a t -> 'b t
end

module Simple : sig
  type view = [
    | `Any
    | `Constant of constant
    | `Tuple of pattern list
    | `Construct of
        Longident.t loc * constructor_description * pattern list
    | `Variant of label * pattern option * row_desc ref
    | `Record of
        (Longident.t loc * label_description * pattern) list * closed_flag
    | `Array of pattern list
    | `Lazy of pattern
    | `Exception of pattern
  ]
  type pattern = view pattern_

  val omega : [> view ] pattern_
end

module Half_simple : sig
  type view = [
    | Simple.view
    | `Or of pattern * pattern * row_desc option
  ]
  type pattern = view pattern_
end

module General : sig
  type view = [
    | Half_simple.view
    | `Var of Ident.t * string loc
    | `Alias of pattern * Ident.t * string loc
  ]
  type pattern = view pattern_

  val view : Typedtree.pattern -> pattern
  val erase : [< view ] pattern_ -> Typedtree.pattern

  val strip_vars : pattern -> Half_simple.pattern

  val assert_simple : pattern -> Simple.pattern
end

module Head : sig
  type desc =
    | Any
    | Construct of constructor_description
    | Constant of constant
    | Tuple of int
    | Record of label_description list
    | Variant of
        { tag: label; has_arg: bool;
          cstr_row: row_desc ref;
          type_row : unit -> row_desc; }
          (* the row of the type may evolve if [close_variant] is called,
             hence the (unit -> ...) delay *)
    | Array of int
    | Lazy

  type t = desc pattern_

  val arity : t -> int

  (** [deconstruct p] returns the head of [p] and the list of sub patterns.

      @raises [Invalid_arg _] if [p] is an or- or an exception-pattern.  *)
  val deconstruct : Simple.pattern -> t * pattern list

  (** reconstructs a pattern, putting wildcards as sub-patterns. *)
  val to_omega_pattern : t -> pattern

  val omega : t

end
