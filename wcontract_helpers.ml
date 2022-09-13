(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Tezos_base.TzPervasives
open Tezos_protocol_014_PtKathma
open Tezos_014_PtKathma_test_helpers

open Protocol
open Alpha_context
open Error_monad_operators

let fake_KT1 =
  Contract_hash.of_b58check_exn "KT1FAKEFAKEFAKEFAKEFAKEFAKEFAKGGSE2x"

let default_self = fake_KT1

let default_source = Contract.Implicit Signature.Public_key_hash.zero

let default_step_constants =
  Script_interpreter.
    {
      source = default_source;
      payer = default_source;
      self = default_self;
      amount = Tez.zero;
      balance = Tez.zero;
      chain_id = Chain_id.zero;
      now = Script_timestamp.of_zint Z.zero;
      level = Script_int.zero_n;
    }

(** Helper function that parses and types a script, its initial storage and
   parameters from strings. It then executes the typed script with the storage
   and parameter and returns the result. *)
let run_script ?logger ctx ?(step_constants = default_step_constants) contract
    ?(entrypoint = Entrypoint.default) ~storage ~parameter () =
  let contract_expr = Expr.from_string contract in
  let storage_expr = Expr.from_string storage in
  let parameter_expr = Expr.from_string parameter in
  let script =
    Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  Script_interpreter.execute
    ?logger
    ctx
    Readable
    step_constants
    ~script
    ~cached_script:None
    ~entrypoint
    ~parameter:parameter_expr
    ~internal:false
  >>=?? fun res -> Result.return res |> Lwt.return
