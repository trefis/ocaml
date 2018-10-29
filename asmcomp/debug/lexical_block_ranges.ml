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

module Lexical_blocks = struct
  module Key = Debuginfo.Current_block
  module Index = Debuginfo.Current_block

  module Subrange_state :
    Compute_ranges_intf.S_subrange_state
  = struct
    type t = unit

    let create () = ()
    let advance_over_instruction () _ = ()
  end

  module Subrange_info :
    Compute_ranges_intf.S_subrange_info
      with type key := Key.t
      with type subrange_state := Subrange_state.t
  = struct
    type t = unit

    let create _var _subrange_state = ()
  end

  module Range_info :
    Compute_ranges_intf.S_range_info
      with type key := Key.t
      with type index := Index.t
  = struct
    type t = unit

    let create _fundecl block ~start_insn:_ = Some (block, ())
  end

  let available_before (insn : L.instruction) =
    Debuginfo.Current_block.Set.singleton (Debuginfo.innermost_block insn.dbg)

  let available_across insn =
    (* Block scoping never changes during the execution of a [Linearize]
       instruction. *)
    available_before insn

  let must_restart_ranges_upon_any_change () = false
end

module Subrange_state = Lexical_blocks.Subrange_state
module Subrange_info = Lexical_blocks.Subrange_info
module Range_info = Lexical_blocks.Range_info

include Compute_ranges.Make (Lexical_blocks)
