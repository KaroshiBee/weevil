open Protocol
open Environment
open Alpha_context
open Environment.Error_monad
module Plugin = Tezos_protocol_plugin_014_PtKathma

module RPC = struct
  module type UNPARSING_MODE = sig
    val unparsing_mode : Script_ir_translator.unparsing_mode
  end

  module Traced_interpreter (Unparsing_mode : UNPARSING_MODE) = struct
    type log_element =
      | Log :
          context
          * Script.location
          * ('a * 's)
          * ('a, 's) Script_typed_ir.stack_ty
          -> log_element

    let unparse_stack ctxt (stack, stack_ty) =
      (* We drop the gas limit as this function is only used for debugging/errors. *)
      let ctxt = Gas.set_unlimited ctxt in
      let rec unparse_stack :
          type a s.
          (a, s) Script_typed_ir.stack_ty * (a * s) ->
          Script.expr list tzresult Lwt.t = function
        | Bot_t, (EmptyCell, EmptyCell) -> return_nil
        | Item_t (ty, rest_ty), (v, rest) ->
            Script_ir_translator.unparse_data
              ctxt
              Unparsing_mode.unparsing_mode
              ty
              v
            >>=? fun (data, _ctxt) ->
            unparse_stack (rest_ty, rest) >|=? fun rest ->
            let data = Micheline.strip_locations data in
            data :: rest
      in
      unparse_stack (stack_ty, stack)

    let trace_logger () : Script_typed_ir.logger =
      let log : log_element list ref = ref [] in
      let log_interp _ ctxt loc sty stack =
        log := Log (ctxt, loc, stack, sty) :: !log
      in
      let log_entry _ _ctxt _loc _sty _stack = () in
      let log_exit _ ctxt loc sty stack =
        log := Log (ctxt, loc, stack, sty) :: !log
      in
      let log_control _ = () in
      let get_log () =
        List.map_es
          (fun (Log (ctxt, loc, stack, stack_ty)) ->
            trace
              Plugin.Plugin_errors.Cannot_serialize_log
              (unparse_stack ctxt (stack, stack_ty))
            >>=? fun stack -> return (loc, Gas.level ctxt, stack))
          !log
        >>=? fun res -> return (Some (List.rev res))
      in
      {log_exit; log_entry; log_interp; get_log; log_control}

    let execute ctxt step_constants ~script ~entrypoint ~parameter =
      let open Script_interpreter in
      let logger = trace_logger () in
      execute
        ~logger
        ~cached_script:None
        ctxt
        Unparsing_mode.unparsing_mode
        step_constants
        ~script
        ~entrypoint
        ~parameter
        ~internal:true
      >>=? fun res ->
      logger.get_log () >|=? fun trace ->
      let trace = Option.value ~default:[] trace in
      (res, trace)
  end

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

  let configure_contracts ctxt script balance ~src_opt ~pay_opt ~self_opt =
    (match self_opt with
     | None ->
       let balance = Option.value ~default:Plugin.RPC.Scripts.default_balance balance in
       originate_dummy_contract ctxt script balance >>=? fun (ctxt, addr) ->
       return (ctxt, addr, balance)
     | Some addr ->
       Plugin.RPC.Scripts.default_from_context
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
    return (ctxt, {Plugin.RPC.Scripts.balance; self; source; payer})



  let trace_code ctxt ()
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
        (unparsing_mode, gas, now, level) ) =
    let unparsing_mode = Option.value ~default:Script_ir_translator.Readable unparsing_mode in
    let storage = Script.lazy_expr storage in
    let code = Script.lazy_expr code in
    configure_contracts ctxt {storage; code} balance ~src_opt ~pay_opt ~self_opt
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
    let module Interp = Traced_interpreter (Unparsing_mode) in
    Interp.execute
      ctxt
      step_constants
      ~script:{storage; code}
      ~entrypoint
      ~parameter
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
      Apply_internal_results.contents_of_packed_internal_operations operations,
      trace,
      lazy_storage_diff )
end
