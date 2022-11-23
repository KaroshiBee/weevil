open Protocol
open Alpha_context
open Environment.Error_monad
module Plugin = Tezos_protocol_plugin_014_PtKathma

let originate_dummy_contract ctxt script balance =
  let ctxt = Origination_nonce.init ctxt Environment.Operation_hash.zero in
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



let trace_code ~msg_mvar ctxt ()
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
  (* NOTE type unparsing_mode = Optimized | Readable | Optimized_legacy *)
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
  let module Interp = Mdb_traced_interpreter.T (Unparsing_mode) in
  let logger = Interp.trace_logger ~msg_mvar () in
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
    Apply_internal_results.contents_of_packed_internal_operations operations,
    trace,
    lazy_storage_diff )
