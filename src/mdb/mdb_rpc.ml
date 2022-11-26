(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Protocol
open Environment
open Alpha_context
open Environment.Error_monad
module Plugin_errors = Tezos_protocol_plugin_014_PtKathma.Plugin_errors
module View_helpers = Tezos_protocol_plugin_014_PtKathma.View_helpers

(** The assumed number of blocks between operation-creation time and
    the actual time when the operation is included in a block. *)
let default_operation_inclusion_latency = 3

let parse_operation (op : Operation.raw) =
  match
    Data_encoding.Binary.of_bytes_opt Operation.protocol_data_encoding op.proto
  with
  | Some protocol_data -> ok {shell = op.shell; protocol_data}
  | None -> error Plugin_errors.Cannot_parse_operation

let path = RPC_path.(open_root / "helpers")

module Registration = struct
  let patched_services =
    ref (RPC_directory.empty : Updater.rpc_context RPC_directory.t)

  let register0_fullctxt ~chunked s f =
    patched_services :=
      RPC_directory.register ~chunked !patched_services s (fun ctxt q i ->
          Services_registration.rpc_init ctxt `Head_level >>=? fun ctxt ->
          f ctxt q i)

  let register0 ~chunked s f =
    register0_fullctxt ~chunked s (fun {context; _} -> f context)

  let register0_fullctxt_successor_level ~chunked s f =
    patched_services :=
      RPC_directory.register ~chunked !patched_services s (fun ctxt q i ->
          let mode =
            if q#successor_level then `Successor_level else `Head_level
          in
          Services_registration.rpc_init ctxt mode >>=? fun ctxt -> f ctxt q i)

  let register0_successor_level ~chunked s f =
    register0_fullctxt_successor_level ~chunked s (fun {context; _} ->
        f context)

  let register0_noctxt ~chunked s f =
    patched_services :=
      RPC_directory.register ~chunked !patched_services s (fun _ q i -> f q i)

  let opt_register0_fullctxt ~chunked s f =
    patched_services :=
      RPC_directory.opt_register ~chunked !patched_services s (fun ctxt q i ->
          Services_registration.rpc_init ctxt `Head_level >>=? fun ctxt ->
          f ctxt q i)

  let opt_register0 ~chunked s f =
    opt_register0_fullctxt ~chunked s (fun {context; _} -> f context)

  let register1_fullctxt ~chunked s f =
    patched_services :=
      RPC_directory.register
        ~chunked
        !patched_services
        s
        (fun (ctxt, arg) q i ->
          Services_registration.rpc_init ctxt `Head_level >>=? fun ctxt ->
          f ctxt arg q i)

  let opt_register1_fullctxt ~chunked s f =
    patched_services :=
      RPC_directory.opt_register
        ~chunked
        !patched_services
        s
        (fun (ctxt, arg) q i ->
          Services_registration.rpc_init ctxt `Head_level >>=? fun ctxt ->
          f ctxt arg q i)

  let register1 ~chunked s f =
    register1_fullctxt ~chunked s (fun {context; _} x -> f context x)

  let opt_register1 ~chunked s f =
    opt_register1_fullctxt ~chunked s (fun {context; _} x -> f context x)

  let register2_fullctxt ~chunked s f =
    patched_services :=
      RPC_directory.register
        ~chunked
        !patched_services
        s
        (fun ((ctxt, arg1), arg2) q i ->
          Services_registration.rpc_init ctxt `Head_level >>=? fun ctxt ->
          f ctxt arg1 arg2 q i)

  let register2 ~chunked s f =
    register2_fullctxt ~chunked s (fun {context; _} a1 a2 q i ->
        f context a1 a2 q i)
end

let unparsing_mode_encoding =
  let open Script_ir_translator in
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        (Tag 0)
        ~title:"Readable"
        (constant "Readable")
        (function Readable -> Some () | Optimized | Optimized_legacy -> None)
        (fun () -> Readable);
      case
        (Tag 1)
        ~title:"Optimized"
        (constant "Optimized")
        (function Optimized -> Some () | Readable | Optimized_legacy -> None)
        (fun () -> Optimized);
      case
        (Tag 2)
        ~title:"Optimized_legacy"
        (constant "Optimized_legacy")
        (function Optimized_legacy -> Some () | Readable | Optimized -> None)
        (fun () -> Optimized_legacy);
    ]

module Scripts = struct
  module S = struct
    open Data_encoding

    let path = RPC_path.(path / "scripts")

    let run_code_input_encoding =
      merge_objs
        (obj10
           (req "script" Script.expr_encoding)
           (req "storage" Script.expr_encoding)
           (req "input" Script.expr_encoding)
           (req "amount" Tez.encoding)
           (opt "balance" Tez.encoding)
           (req "chain_id" Chain_id.encoding)
           (opt "source" Contract.encoding)
           (opt "payer" Contract.encoding)
           (opt "self" Contract.originated_encoding)
           (dft "entrypoint" Entrypoint.simple_encoding Entrypoint.default))
        (obj4
           (opt "unparsing_mode" unparsing_mode_encoding)
           (opt "gas" Gas.Arith.z_integral_encoding)
           (opt "now" Script_timestamp.encoding)
           (opt "level" Script_int.n_encoding))

    let run_code_output_encoding =
      conv
        (fun (storage, operations, lazy_storage_diff) ->
          (storage, operations, lazy_storage_diff))
        (fun (storage, operations, lazy_storage_diff) ->
          (storage, operations, lazy_storage_diff))
        (obj3
           (req "storage" Script.expr_encoding)
           (req
              "operations"
              (list Apply_internal_results.internal_contents_encoding))
           (opt "lazy_storage_diff" Lazy_storage.encoding))

    let trace_code_input_encoding = run_code_input_encoding

    let trace_encoding =
      def "scripted.trace" @@ list
      @@ obj3
           (req "location" Script.location_encoding)
           (req "gas" Gas.encoding)
           (req "stack" (list Script.expr_encoding))

    let trace_code_output_encoding =
      conv
        (fun (storage, operations, trace, lazy_storage_diff) ->
          (storage, operations, trace, lazy_storage_diff))
        (fun (storage, operations, trace, lazy_storage_diff) ->
          (storage, operations, trace, lazy_storage_diff))
        (obj4
           (req "storage" Script.expr_encoding)
           (req
              "operations"
              (list Apply_internal_results.internal_contents_encoding))
           (req "trace" trace_encoding)
           (opt "lazy_storage_diff" Lazy_storage.encoding))


    let trace_code =
      RPC_service.post_service
        ~description:
          "Run a piece of code in the current context, keeping a trace"
        ~query:RPC_query.empty
        ~input:trace_code_input_encoding
        ~output:trace_code_output_encoding
        RPC_path.(path / "trace_code")


    let typecheck_code =
      RPC_service.post_service
        ~description:"Typecheck a piece of code in the current context"
        ~query:RPC_query.empty
        ~input:
          (obj4
             (req "program" Script.expr_encoding)
             (opt "gas" Gas.Arith.z_integral_encoding)
             (opt "legacy" bool)
             (opt "show_types" bool))
        ~output:
          (obj2
             (req "type_map" Script_tc_errors_registration.type_map_enc)
             (req "gas" Gas.encoding))
        RPC_path.(path / "typecheck_code")

    let typecheck_data =
      RPC_service.post_service
        ~description:
          "Check that some data expression is well formed and of a given type \
           in the current context"
        ~query:RPC_query.empty
        ~input:
          (obj4
             (req "data" Script.expr_encoding)
             (req "type" Script.expr_encoding)
             (opt "gas" Gas.Arith.z_integral_encoding)
             (opt "legacy" bool))
        ~output:(obj1 (req "gas" Gas.encoding))
        RPC_path.(path / "typecheck_data")

  end

  module type UNPARSING_MODE = sig
    val unparsing_mode : Script_ir_translator.unparsing_mode
  end


  let typecheck_data :
      legacy:bool ->
      context ->
      Script.expr * Script.expr ->
      context tzresult Lwt.t =
   fun ~legacy ctxt (data, exp_ty) ->
    record_trace
      (Script_tc_errors.Ill_formed_type (None, exp_ty, 0))
      (Script_ir_translator.parse_passable_ty
         ctxt
         ~legacy
         (Micheline.root exp_ty))
    >>?= fun (Ex_ty exp_ty, ctxt) ->
    trace_eval
      (fun () ->
        let exp_ty = Script_ir_translator.serialize_ty_for_error exp_ty in
        Script_tc_errors.Ill_typed_data (None, data, exp_ty))
      (let allow_forged =
         true
         (* Safe since we ignore the value afterwards. *)
       in
       Script_ir_translator.parse_data
         ctxt
         ~legacy
         ~allow_forged
         exp_ty
         (Micheline.root data))
    >|=? fun (_, ctxt) -> ctxt

  module Unparse_types = struct
    (* Same as the unparsing functions for types in Script_ir_translator but
       does not consume gas and never folds (pair a (pair b c)) *)

    open Micheline
    open Michelson_v1_primitives
    open Script_typed_ir

    let unparse_memo_size ~loc memo_size =
      let z = Alpha_context.Sapling.Memo_size.unparse_to_z memo_size in
      Int (loc, z)

    let rec unparse_ty :
        type a ac loc.
        loc:loc -> (a, ac) ty -> (loc, Script.prim) Micheline.node =
     fun ~loc ty ->
      let return (name, args, annot) = Prim (loc, name, args, annot) in
      match ty with
      | Unit_t -> return (T_unit, [], [])
      | Int_t -> return (T_int, [], [])
      | Nat_t -> return (T_nat, [], [])
      | Signature_t -> return (T_signature, [], [])
      | String_t -> return (T_string, [], [])
      | Bytes_t -> return (T_bytes, [], [])
      | Mutez_t -> return (T_mutez, [], [])
      | Bool_t -> return (T_bool, [], [])
      | Key_hash_t -> return (T_key_hash, [], [])
      | Key_t -> return (T_key, [], [])
      | Timestamp_t -> return (T_timestamp, [], [])
      | Address_t -> return (T_address, [], [])
      | Tx_rollup_l2_address_t -> return (T_tx_rollup_l2_address, [], [])
      | Operation_t -> return (T_operation, [], [])
      | Chain_id_t -> return (T_chain_id, [], [])
      | Never_t -> return (T_never, [], [])
      | Bls12_381_g1_t -> return (T_bls12_381_g1, [], [])
      | Bls12_381_g2_t -> return (T_bls12_381_g2, [], [])
      | Bls12_381_fr_t -> return (T_bls12_381_fr, [], [])
      | Contract_t (ut, _meta) ->
          let t = unparse_ty ~loc ut in
          return (T_contract, [t], [])
      | Pair_t (utl, utr, _meta, _) ->
          let annot = [] in
          let tl = unparse_ty ~loc utl in
          let tr = unparse_ty ~loc utr in
          return (T_pair, [tl; tr], annot)
      | Union_t (utl, utr, _meta, _) ->
          let annot = [] in
          let tl = unparse_ty ~loc utl in
          let tr = unparse_ty ~loc utr in
          return (T_or, [tl; tr], annot)
      | Lambda_t (uta, utr, _meta) ->
          let ta = unparse_ty ~loc uta in
          let tr = unparse_ty ~loc utr in
          return (T_lambda, [ta; tr], [])
      | Option_t (ut, _meta, _) ->
          let annot = [] in
          let ut = unparse_ty ~loc ut in
          return (T_option, [ut], annot)
      | List_t (ut, _meta) ->
          let t = unparse_ty ~loc ut in
          return (T_list, [t], [])
      | Ticket_t (ut, _meta) ->
          let t = unparse_ty ~loc ut in
          return (T_ticket, [t], [])
      | Set_t (ut, _meta) ->
          let t = unparse_ty ~loc ut in
          return (T_set, [t], [])
      | Map_t (uta, utr, _meta) ->
          let ta = unparse_ty ~loc uta in
          let tr = unparse_ty ~loc utr in
          return (T_map, [ta; tr], [])
      | Big_map_t (uta, utr, _meta) ->
          let ta = unparse_ty ~loc uta in
          let tr = unparse_ty ~loc utr in
          return (T_big_map, [ta; tr], [])
      | Sapling_transaction_t memo_size ->
          return (T_sapling_transaction, [unparse_memo_size ~loc memo_size], [])
      | Sapling_transaction_deprecated_t memo_size ->
          return
            ( T_sapling_transaction_deprecated,
              [unparse_memo_size ~loc memo_size],
              [] )
      | Sapling_state_t memo_size ->
          return (T_sapling_state, [unparse_memo_size ~loc memo_size], [])
      | Chest_t -> return (T_chest, [], [])
      | Chest_key_t -> return (T_chest_key, [], [])
  end

  let rec pp_instr_name :
      type a b c d.
      Format.formatter -> (a, b, c, d) Script_typed_ir.kinstr -> unit =
    let open Script_typed_ir in
    let open Format in
    fun fmt -> function
      | IDrop _ -> pp_print_string fmt "DROP"
      | IDup _ -> pp_print_string fmt "DUP"
      | ISwap _ -> pp_print_string fmt "SWAP"
      | IConst _ -> pp_print_string fmt "CONST"
      | ICons_pair _ -> pp_print_string fmt "PAIR"
      | ICar _ -> pp_print_string fmt "CAR"
      | ICdr _ -> pp_print_string fmt "CDR"
      | IUnpair _ -> pp_print_string fmt "UNPAIR"
      | ICons_some _ -> pp_print_string fmt "SOME"
      | ICons_none _ -> pp_print_string fmt "NONE"
      | IIf_none _ -> pp_print_string fmt "IF_NONE"
      | IOpt_map _ -> pp_print_string fmt "MAP"
      | ICons_left _ -> pp_print_string fmt "LEFT"
      | ICons_right _ -> pp_print_string fmt "RIGHT"
      | IIf_left _ -> pp_print_string fmt "IF_LEFT"
      | ICons_list _ -> pp_print_string fmt "CONS"
      | INil _ -> pp_print_string fmt "NIL"
      | IIf_cons _ -> pp_print_string fmt "IF_CONS"
      | IList_map _ -> pp_print_string fmt "MAP"
      | IList_iter _ -> pp_print_string fmt "ITER"
      | IList_size _ -> pp_print_string fmt "SIZE"
      | IEmpty_set _ -> pp_print_string fmt "EMPTY_SET"
      | ISet_iter _ -> pp_print_string fmt "ITER"
      | ISet_mem _ -> pp_print_string fmt "MEM"
      | ISet_update _ -> pp_print_string fmt "UPDATE"
      | ISet_size _ -> pp_print_string fmt "SIZE"
      | IEmpty_map _ -> pp_print_string fmt "EMPTY_MAP"
      | IMap_map _ -> pp_print_string fmt "MAP"
      | IMap_iter _ -> pp_print_string fmt "ITER"
      | IMap_mem _ -> pp_print_string fmt "MEM"
      | IMap_get _ -> pp_print_string fmt "GET"
      | IMap_update _ -> pp_print_string fmt "UPDATE"
      | IMap_get_and_update _ -> pp_print_string fmt "GET_AND_UPDATE"
      | IMap_size _ -> pp_print_string fmt "SIZE"
      | IEmpty_big_map _ -> pp_print_string fmt "EMPTY_BIG_MAP"
      | IBig_map_mem _ -> pp_print_string fmt "MEM"
      | IBig_map_get _ -> pp_print_string fmt "GET"
      | IBig_map_update _ -> pp_print_string fmt "UPDATE"
      | IBig_map_get_and_update _ -> pp_print_string fmt "GET_AND_UPDATE"
      | IConcat_string _ -> pp_print_string fmt "CONCAT"
      | IConcat_string_pair _ -> pp_print_string fmt "CONCAT"
      | ISlice_string _ -> pp_print_string fmt "SLICE"
      | IString_size _ -> pp_print_string fmt "SIZE"
      | IConcat_bytes _ -> pp_print_string fmt "CONCAT"
      | IConcat_bytes_pair _ -> pp_print_string fmt "CONCAT"
      | ISlice_bytes _ -> pp_print_string fmt "SLICE"
      | IBytes_size _ -> pp_print_string fmt "SIZE"
      | IAdd_seconds_to_timestamp _ -> pp_print_string fmt "ADD"
      | IAdd_timestamp_to_seconds _ -> pp_print_string fmt "ADD"
      | ISub_timestamp_seconds _ -> pp_print_string fmt "SUB"
      | IDiff_timestamps _ -> pp_print_string fmt "DIFF"
      | IAdd_tez _ -> pp_print_string fmt "ADD"
      | ISub_tez _ -> pp_print_string fmt "SUB_MUTEZ"
      | ISub_tez_legacy _ -> pp_print_string fmt "SUB"
      | IMul_teznat _ | IMul_nattez _ -> pp_print_string fmt "MUL"
      | IEdiv_teznat _ -> pp_print_string fmt "EDIV"
      | IEdiv_tez _ -> pp_print_string fmt "EDIV"
      | IOr _ -> pp_print_string fmt "OR"
      | IAnd _ -> pp_print_string fmt "AND"
      | IXor _ -> pp_print_string fmt "XOR"
      | INot _ -> pp_print_string fmt "NOT"
      | IIs_nat _ -> pp_print_string fmt "ISNAT"
      | INeg _ -> pp_print_string fmt "NEG"
      | IAbs_int _ -> pp_print_string fmt "ABS"
      | IInt_nat _ -> pp_print_string fmt "INT"
      | IAdd_int _ | IAdd_nat _ -> pp_print_string fmt "ADD"
      | ISub_int _ -> pp_print_string fmt "SUB"
      | IMul_int _ | IMul_nat _ -> pp_print_string fmt "MUL"
      | IEdiv_int _ | IEdiv_nat _ -> pp_print_string fmt "EDIV"
      | ILsl_nat _ -> pp_print_string fmt "LSL"
      | ILsr_nat _ -> pp_print_string fmt "LSR"
      | IOr_nat _ -> pp_print_string fmt "OR"
      | IAnd_nat _ -> pp_print_string fmt "AND"
      | IAnd_int_nat _ -> pp_print_string fmt "AND"
      | IXor_nat _ -> pp_print_string fmt "XOR"
      | INot_int _ -> pp_print_string fmt "NOT"
      | IIf _ -> pp_print_string fmt "IF"
      | ILoop _ -> pp_print_string fmt "LOOP"
      | ILoop_left _ -> pp_print_string fmt "LOOP_LEFT"
      | IDip _ -> pp_print_string fmt "DIP"
      | IExec _ -> pp_print_string fmt "EXEC"
      | IApply _ -> pp_print_string fmt "APPLY"
      | ILambda _ -> pp_print_string fmt "LAMBDA"
      | IFailwith _ -> pp_print_string fmt "FAILWITH"
      | ICompare _ -> pp_print_string fmt "COMPARE"
      | IEq _ -> pp_print_string fmt "EQ"
      | INeq _ -> pp_print_string fmt "NEQ"
      | ILt _ -> pp_print_string fmt "LT"
      | IGt _ -> pp_print_string fmt "GT"
      | ILe _ -> pp_print_string fmt "LE"
      | IGe _ -> pp_print_string fmt "GE"
      | IAddress _ -> pp_print_string fmt "ADDRESS"
      | IContract _ -> pp_print_string fmt "CONTACT"
      | IView _ -> pp_print_string fmt "VIEW"
      | ITransfer_tokens _ -> pp_print_string fmt "TRANSFER_TOKENS"
      | IImplicit_account _ -> pp_print_string fmt "IMPLICIT_ACCOUNT"
      | ICreate_contract _ -> pp_print_string fmt "CREATE_CONTRACT"
      | ISet_delegate _ -> pp_print_string fmt "SET_DELEGATE"
      | INow _ -> pp_print_string fmt "NOW"
      | IMin_block_time _ -> pp_print_string fmt "MIN_BLOCK_TIME"
      | IBalance _ -> pp_print_string fmt "BALANCE"
      | ILevel _ -> pp_print_string fmt "LEVEL"
      | ICheck_signature _ -> pp_print_string fmt "CHECK_SIGNATURE"
      | IHash_key _ -> pp_print_string fmt "HASH_KEY"
      | IPack _ -> pp_print_string fmt "PACK"
      | IBlake2b _ -> pp_print_string fmt "BLAKE2B"
      | ISha3 _ -> pp_print_string fmt "SHA3"
      | ISha256 _ -> pp_print_string fmt "SHA256"
      | ISha512 _ -> pp_print_string fmt "SHA512"
      | IUnpack _ -> pp_print_string fmt "UNPACK"
      | ISource _ -> pp_print_string fmt "SOURCE"
      | ISender _ -> pp_print_string fmt "SENDER"
      | ISelf _ -> pp_print_string fmt "SELF"
      | ISelf_address _ -> pp_print_string fmt "SELF_ADDRESS"
      | IAmount _ -> pp_print_string fmt "AMOUNT"
      | ISapling_empty_state _ -> pp_print_string fmt "SAPLING_EMPTY_STATE"
      | ISapling_verify_update _ | ISapling_verify_update_deprecated _ ->
          pp_print_string fmt "SAPLING_VERIFY_UPDATE"
      | IDig _ -> pp_print_string fmt "DIG"
      | IDug _ -> pp_print_string fmt "DUG"
      | IDipn _ -> pp_print_string fmt "DIP"
      | IDropn _ -> pp_print_string fmt "DROP"
      | IChainId _ -> pp_print_string fmt "CHAIN_ID"
      | INever _ -> pp_print_string fmt "NEVER"
      | IVoting_power _ -> pp_print_string fmt "VOTING_POWER"
      | ITotal_voting_power _ -> pp_print_string fmt "TOTAL_VOTING_POWER"
      | IKeccak _ -> pp_print_string fmt "KECCAK"
      | IAdd_bls12_381_g1 _ | IAdd_bls12_381_g2 _ | IAdd_bls12_381_fr _ ->
          pp_print_string fmt "ADD"
      | IMul_bls12_381_g1 _ | IMul_bls12_381_g2 _ | IMul_bls12_381_fr _
      | IMul_bls12_381_z_fr _ | IMul_bls12_381_fr_z _ ->
          pp_print_string fmt "MUL"
      | IInt_bls12_381_fr _ -> pp_print_string fmt "INT"
      | INeg_bls12_381_g1 _ | INeg_bls12_381_g2 _ | INeg_bls12_381_fr _ ->
          pp_print_string fmt "NEG"
      | IPairing_check_bls12_381 _ -> pp_print_string fmt "PAIRING_CHECK"
      | IComb _ -> pp_print_string fmt "PAIR"
      | IUncomb _ -> pp_print_string fmt "UNPAIR"
      | IComb_get _ -> pp_print_string fmt "GET"
      | IComb_set _ -> pp_print_string fmt "UPDATE"
      | IDup_n _ -> pp_print_string fmt "DUP"
      | ITicket _ -> pp_print_string fmt "TICKET"
      | IRead_ticket _ -> pp_print_string fmt "READ_TICKET"
      | ISplit_ticket _ -> pp_print_string fmt "SPLIT_TICKET"
      | IJoin_tickets _ -> pp_print_string fmt "JOIN_TICKETS"
      | IOpen_chest _ -> pp_print_string fmt "OPEN_CHEST"
      | IEmit _ -> pp_print_string fmt "EMIT"
      | IHalt _ -> pp_print_string fmt "[halt]"
      | ILog (_, _, _, _, instr) ->
          Format.fprintf fmt "log/%a" pp_instr_name instr


  let default_from_context ctxt get = function
    | None -> get ctxt
    | Some x -> return x

  (* A convenience type for return values of [ensure_contracts_exist] below. *)
  type run_code_config = {
    balance : Tez.t;
    self : Contract_hash.t;
    payer : Contract.t;
    source : Contract.t;
  }

  (* 4_000_000 ꜩ *)
  let default_balance = Tez.of_mutez_exn 4_000_000_000_000L

  let register () =
    let originate_dummy_contract ctxt script balance =
      let ctxt = Origination_nonce.init ctxt Operation_hash.zero in
      Contract.fresh_contract_from_current_nonce ctxt
      >>?= fun (ctxt, dummy_contract_hash) ->
      let dummy_contract = Contract.Originated dummy_contract_hash in
      Contract.raw_originate
        ctxt
        ~prepaid_bootstrap_storage:false
        dummy_contract_hash
        ~script:(script, None)
      >>=? fun ctxt ->
      Token.transfer
        ~origin:Simulation
        ctxt
        `Minted
        (`Contract dummy_contract)
        balance
      >>=? fun (ctxt, _) -> return (ctxt, dummy_contract_hash)
    in
    let configure_contracts ctxt script balance ~src_opt ~pay_opt ~self_opt =
      (match self_opt with
      | None ->
          let balance = Option.value ~default:default_balance balance in
          originate_dummy_contract ctxt script balance >>=? fun (ctxt, addr) ->
          return (ctxt, addr, balance)
      | Some addr ->
          default_from_context
            ctxt
            (fun c -> Contract.get_balance c @@ Contract.Originated addr)
            balance
          >>=? fun bal -> return (ctxt, addr, bal))
      >>=? fun (ctxt, self, balance) ->
      let source, payer =
        match (src_opt, pay_opt) with
        | None, None ->
            let self = Contract.Originated self in
            (self, self)
        | Some c, None | None, Some c -> (c, c)
        | Some src, Some pay -> (src, pay)
      in
      return (ctxt, {balance; self; source; payer})
    in
    Registration.register0
      ~chunked:true
      S.trace_code
      (fun
        ctxt
        ()
        ( ( code,
            storage,
            parameter,
            amount,
            balance,
            chain_id,
            src_opt,
            pay_opt,
            self_opt,
            entrypoint ),
          (unparsing_mode, gas, now, level) )
        ->
        let () = Printf.printf "trace code: register" in
        let unparsing_mode = Option.value ~default:Readable unparsing_mode in
        let storage = Script.lazy_expr storage in
        let code = Script.lazy_expr code in
        configure_contracts
          ctxt
          {storage; code}
          balance
          ~src_opt
          ~pay_opt
          ~self_opt
        >>=? fun (ctxt, {self; source; payer; balance}) ->
        let gas =
          match gas with
          | Some gas -> gas
          | None -> Constants.hard_gas_limit_per_operation ctxt
        in
        let ctxt = Gas.set_limit ctxt gas in
        let now =
          match now with None -> Script_timestamp.now ctxt | Some t -> t
        in
        let level =
          match level with
          | None ->
              (Level.current ctxt).level |> Raw_level.to_int32
              |> Script_int.of_int32 |> Script_int.abs
          | Some z -> z
        in
        let step_constants =
          let open Script_interpreter in
          {source; payer; self; amount; balance; chain_id; now; level}
        in
        let module Unparsing_mode = struct
          let unparsing_mode = unparsing_mode
        end in
        let module Interp = Mdb_traced_interpreter.T (Unparsing_mode) in
        let logger = Interp.trace_logger () in
        Interp.execute
          ctxt
          step_constants
          ~script:{storage; code}
          ~entrypoint
          ~parameter
          ~logger
        >|=? fun ( ( {
                       script = _;
                       code_size = _;
                       Script_interpreter.storage;
                       operations;
                       lazy_storage_diff;
                       ticket_diffs = _;
                     },
                     _ctxt ),
                   trace ) ->
        ( storage,
          Apply_internal_results.contents_of_packed_internal_operations
            operations,
          trace,
          lazy_storage_diff )) ;
    Registration.register0
      ~chunked:false
      S.typecheck_code
      (fun ctxt () (expr, maybe_gas, legacy, show_types) ->
        let legacy = Option.value ~default:false legacy in
        let show_types = Option.value ~default:true show_types in
        let ctxt =
          match maybe_gas with
          | None -> Gas.set_unlimited ctxt
          | Some gas -> Gas.set_limit ctxt gas
        in
        Script_ir_translator.typecheck_code ~legacy ~show_types ctxt expr
        >|=? fun (res, ctxt) -> (res, Gas.level ctxt)) ;

    Registration.register0
      ~chunked:false
      S.typecheck_data
      (fun ctxt () (data, ty, maybe_gas, legacy) ->
        let legacy = Option.value ~default:false legacy in
        let ctxt =
          match maybe_gas with
          | None -> Gas.set_unlimited ctxt
          | Some gas -> Gas.set_limit ctxt gas
        in
        typecheck_data ~legacy ctxt (data, ty) >|=? fun ctxt -> Gas.level ctxt)

  let trace_code ?unparsing_mode ?gas ?(entrypoint = Entrypoint.default)
      ?balance ~script ~storage ~input ~amount ~chain_id ~source ~payer ~self
      ~now ~level ctxt block =
    Printf.printf "lib_plugin.RPC: trace code call with make_call0\n";
    RPC_context.make_call0
      S.trace_code
      ctxt
      block
      ()
      ( ( script,
          storage,
          input,
          amount,
          balance,
          chain_id,
          source,
          payer,
          self,
          entrypoint ),
        (unparsing_mode, gas, now, level) )


  let typecheck_code ?gas ?legacy ~script ?show_types ctxt block =
    RPC_context.make_call0
      S.typecheck_code
      ctxt
      block
      ()
      (script, gas, legacy, show_types)

  let typecheck_data ?gas ?legacy ~data ~ty ctxt block =
    RPC_context.make_call0 S.typecheck_data ctxt block () (data, ty, gas, legacy)

end

type Environment.Error_monad.error += Negative_level_offset

let () =
  Environment.Error_monad.register_error_kind
    `Permanent
    ~id:"negative_level_offset"
    ~title:"The specified level offset is negative"
    ~description:"The specified level offset is negative"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "The specified level offset should be positive.")
    Data_encoding.unit
    (function Negative_level_offset -> Some () | _ -> None)
    (fun () -> Negative_level_offset)

let register () =
  Printf.printf "registering RPC stuff\n";
  Scripts.register ()

let rpc_services =
  register () ;
  RPC_directory.merge rpc_services !Registration.patched_services
