(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013, Jane Street Holding                                *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

type t =
  | DW_op_regx of Value.t

let register ~reg_number ~offset:_ =
  let reg_number = Value.as_uleb128 reg_number in
  DW_op_regx reg_number

let opcode = function
  | DW_op_regx _ -> 0x90

let size t =
  let opcode_size = 1 in
  let args_size =
    match t with
    | DW_op_regx reg_number -> Value.size reg_number
  in
  opcode_size + args_size

let emit t ~emitter =
  Value.emit (Value.as_byte (opcode t)) ~emitter;
  match t with
  | DW_op_regx reg_number -> Value.emit reg_number ~emitter
