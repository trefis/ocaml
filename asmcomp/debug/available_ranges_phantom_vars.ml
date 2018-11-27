(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module L = Linearize

module Phantom_vars = struct
  module Key = struct
    include Backend_var
    let all_parents _t = []
  end

  module Index = Backend_var

  module Subrange_state :
    Compute_ranges_intf.S_subrange_state
  = struct
    type t = unit

    let create () = ()
    let advance_over_instruction _ _ = ()
  end

  module Subrange_info :
    Compute_ranges_intf.S_subrange_info
      with type key := Key.t
      with type subrange_state := Subrange_state.t
  = struct
    type t = unit

    let create _var _subrange_state = ()
  end

  module Range_info : sig
    include Compute_ranges_intf.S_range_info
      with type key := Key.t
      with type index := Index.t

    val provenance : t -> Backend_var.Provenance.t option
    val is_parameter : t -> Is_parameter.t
    val defining_expr : t -> Mach.phantom_defining_expr
  end = struct
    type t = {
      provenance : Backend_var.Provenance.t option;
      is_parameter : Is_parameter.t;
      defining_expr : Mach.phantom_defining_expr;
    }

    let create (fundecl : L.fundecl) var ~start_insn:_ =
      match Backend_var.Map.find var fundecl.fun_phantom_lets with
      | exception Not_found ->
        Misc.fatal_errorf "Available_ranges_phantom_vars.Range_info.create: \
            phantom variable occurs in [phantom_available_before] but not in \
            [fun_phantom_lets]: %a"
          Backend_var.print var
      | provenance, defining_expr ->
        (* CR-someday mshinwell: [Local] should presumably change to
           [Parameter] when we emit DWARF inlined function information. *)
        let t =
          { provenance;
            is_parameter = Local;
            defining_expr;
          }
        in
        Some (var, t)

    let provenance t = t.provenance
    let is_parameter t = t.is_parameter
    let defining_expr t = t.defining_expr
  end

  let available_before (insn : L.instruction) =
    insn.phantom_available_before

  let available_across insn =
    (* Phantom variable availability never changes during the execution
       of a [Linearize] instruction. *)
    available_before insn

  let must_restart_ranges_upon_any_change () =
    (* See [Available_ranges_vars]. *)
    match !Clflags.debug_full with
    | Some Gdb -> false
    | Some Lldb -> true
    | None -> Misc.fatal_error "Shouldn't be here without [debug_full]"
end

module Subrange_state = Phantom_vars.Subrange_state
module Subrange_info = Phantom_vars.Subrange_info
module Range_info = Phantom_vars.Range_info

include Compute_ranges.Make (Phantom_vars)
