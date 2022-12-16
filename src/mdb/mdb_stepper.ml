open Protocol
open Alpha_context
module Plugin = Tezos_protocol_plugin_014_PtKathma
module RPC = Tezos_rpc_http_client_unix.RPC_client_unix (* TODO use lib_mockup one? *)
module Client_context_unix = Tezos_client_base_unix.Client_context_unix
module Client_context = Tezos_client_base.Client_context


module T (Interp : Mdb_types.INTERPRETER) = struct

  type code_trace = (Script.expr * Apply_internal_results.packed_internal_contents trace * Lazy_storage.diffs option)

  type t = {
    chain_id:Chain_id.t;
    alpha_context:Alpha_context.t;
    mock_context:Client_context_unix.unix_mockup;
    code_trace:code_trace option;
  }

  type logger = Interp.t
  type expansion_table = Interp.expansion_table

  let code_trace t = t.code_trace
  let chain_id t = t.chain_id
  let alpha_context t = t.alpha_context
  let mock_context t = t.mock_context

  let originate_dummy_contract ctxt script balance =
    let open Lwt_result_syntax in
    let ctxt = Origination_nonce.init ctxt Environment.Operation_hash.zero in
    let* (ctxt, dummy_contract_hash) =
      Lwt.return @@
      Environment.wrap_tzresult @@
      Contract.fresh_contract_from_current_nonce ctxt
    in
    let dummy_contract = Contract.Originated dummy_contract_hash in
    let* ctxt =
      Contract.raw_originate
        ctxt
        ~prepaid_bootstrap_storage:false
        dummy_contract_hash
        ~script:(script, None)
      |> Lwt.map Environment.wrap_tzresult
    in
    let* (ctxt, _) =
      Token.transfer
        ~origin:Simulation
        ctxt
        `Minted
        (`Contract dummy_contract)
        balance
      |> Lwt.map Environment.wrap_tzresult
    in
    return (ctxt, dummy_contract_hash)

  let configure_contracts ctxt script balance ~src_opt ~pay_opt ~self_opt =
    let open Lwt_result_syntax in
    let* (ctxt, self, balance) =
      (match self_opt with
       | None ->
         let balance =
           Option.value ~default:Plugin.RPC.Scripts.default_balance balance
         in
         let* (ctxt, addr) =
           originate_dummy_contract ctxt script balance
         in
         return (ctxt, addr, balance)
       | Some addr ->
         let* bal =
           Plugin.RPC.Scripts.default_from_context
             ctxt
             (fun c -> Contract.get_balance c @@ Contract.Originated addr)
             balance
           |> Lwt.map Environment.wrap_tzresult
         in
         return (ctxt, addr, bal))
    in
    let (source, payer) =
      match (src_opt, pay_opt) with
      | (None, None) ->
        let self = Contract.Originated self in
        (self, self)
      | (Some c, None) | (None, Some c) -> (c, c)
      | (Some src, Some pay) -> (src, pay)
    in
    let run_code_config = Plugin.RPC.Scripts.{balance; self; source; payer} in
    return (ctxt, run_code_config)


  (* TODO move all these args to API call points *)
  let trace_code
      ?gas
      ?(entrypoint = Entrypoint.default)
      ?balance
      ~script
      ~storage
      ~input
      ~expansion_table
      ?(amount = Tez.fifty_cents)
      ?(chain_id = Chain_id.zero)
      ?source
      ?payer
      ?self
      ?now
      ?level
      ~make_logger
      ctxt =

    let open Lwt_result_syntax in

    (* NOTE the RPC call to this also has block parameter after ctxt and type of ctxt is
       'pr #Environment.RPC_context.simple where type of block is 'pr *)
    let*! () = Logs_lwt.debug (fun m -> m "getting storage and code") in
    let logger = make_logger expansion_table in
    let storage = Script.lazy_expr storage in
    let code = Script.lazy_expr script in
    let*! () = Logs_lwt.debug (fun m -> m "getting ctxt config from configure contracts") in
    let* ctxt_config =
      configure_contracts
        ctxt
        {storage; code}
        balance
        ~src_opt:source
        ~pay_opt:payer
        ~self_opt:self
    in
    let (ctxt, Plugin.RPC.Scripts.{balance; self; source; payer}) = ctxt_config in
    let*! () = Logs_lwt.debug (fun m -> m "getting gas") in
    let gas =
      match gas with
      | Some gas -> gas
      | None ->
        Gas.Arith.integral_of_int_exn
          100 (* Constants.hard_gas_limit_per_operation ctxt *)
    in
    let*! () = Logs_lwt.debug (fun m -> m "setting gas limit") in
    let ctxt = Gas.set_limit ctxt gas in
    let*! () = Logs_lwt.debug (fun m -> m "getting now timestamp") in
    let now = match now with None -> Script_timestamp.now ctxt | Some t -> t in
    let*! () = Logs_lwt.debug (fun m -> m "getting level") in
    let level =
      match level with
      | None ->
        (Level.current ctxt).level |> Raw_level.to_int32 |> Script_int.of_int32
        |> Script_int.abs
      | Some z -> z
    in
    let*! () = Logs_lwt.debug (fun m -> m "getting step constants") in
    let step_constants =
      let open Script_interpreter in
      {source; payer; self; amount; balance; chain_id; now; level}
    in

    let*! () = Logs_lwt.debug (fun m -> m "executing contract") in
    (* NOTE in the old code this was a Lwt_result.map - so wouldnt need the return() at end of code block *)
    let* res = Interp.execute
      ctxt
      step_constants
      ~script:{storage; code}
      ~entrypoint
      ~parameter:input
      ~logger
    |> Lwt.map Environment.wrap_tzresult
    in

    let*! () = Logs_lwt.debug (fun m -> m "executed all of contract") in
    let ( Script_interpreter.{
            script = _;
            code_size = _;
            storage;
            operations;
            lazy_storage_diff;
            ticket_diffs = _;
          }, _ctxt ) = res
    in

    let ops = Apply_internal_results.contents_of_packed_internal_operations operations in

    return (
      storage,
      ops,
      lazy_storage_diff
    )


  let init ~protocol_str ~base_dir () =

    let open Lwt_result_syntax in

    Random.self_init ();

    (* believe this RPC config gets overwritten in mock creation, same for the #full cctxt? *)
    let*! () = Logs_lwt.debug (fun m -> m "making rpc config") in
    let rpc_config = RPC.{media_type=Any; endpoint=Uri.empty; logger=null_logger} in
    let*! () = Logs_lwt.debug (fun m -> m "making client context unix full") in
    let cctxt =
      new Client_context_unix.unix_full
        ~chain:`Test
        ~block:(`Head 0)
        ~confirmations:None
        ~password_filename:None
        ~base_dir
        ~rpc_config
        ~verbose_rpc_error_diagnostics:true
    in

    let*! () = Logs_lwt.debug (fun m -> m "making client context unix mockup") in
    let protocol_hash = Protocol_hash.of_b58check_opt protocol_str in
    let* {chain_id; rpc_context; unix_mockup=mock_context} = Mdb_stepper_config.setup_mockup_rpc_client_config
        (cctxt :> Client_context.printer)
        protocol_hash
        base_dir
    in


    let timestamp = rpc_context.block_header.timestamp in
    let level = Int32.succ rpc_context.block_header.level in (* `Successor_level is safer? *)
    let* (alpha_context, _, _) =
      Protocol.Alpha_context.prepare
        ~level
        ~predecessor_timestamp:timestamp
        ~timestamp
        rpc_context.context
      |> Lwt.map Environment.wrap_tzresult
    in
    (* let*! () = Logs_lwt.debug (fun m -> m "doing client config init mockup - ONLY NEEDED FOR DISK BASED MOCKS") *)
    (* let* () = Client_config.config_init_mockup *)
    (*     unix_mockup *)
    (*     protocol_hash *)
    (*     bootstrap_accounts_filename *)
    (*     protocol_constants_filename *)
    (*     base_dir *)
    (* in *)


    (* TODO not sure if we need a remote signer in mock mode *)
    (* let*! () = Logs_lwt.debug (fun m -> m "setup remote signer with mock client config\n") *)
    (* setup_remote_signer *)
    (*   (module C) *)
    (*   client_config *)
    (*   rpc_config *)
    (*   parsed_config_file ; *)

    return {chain_id; alpha_context; mock_context; code_trace=None}

  let typecheck ~script_filename ~storage ~input t =

    let open Lwt_result_syntax in

    let*! () = Logs_lwt.debug (fun m -> m "reading contract file") in
    let* source = t.mock_context#read_file script_filename in
    let*! () = Logs_lwt.debug (fun m -> m "parsing contract source") in
    let*? script = Mdb_typechecker.of_source source in
    let*! () = Logs_lwt.debug (fun m -> m "parsing storage") in
    let*? storage = Mdb_typechecker.from_string storage in
    let*! () = Logs_lwt.debug (fun m -> m "parsing input") in
    let*? input = Mdb_typechecker.from_string input in
    return (script, storage, input)


  let step ~make_logger ~(script:Mdb_typechecker.t) ~(storage:Mdb_typechecker.t) ~(input:Mdb_typechecker.t) t =

    let open Lwt_result_syntax in

    let*! () = Logs_lwt.debug (fun m -> m "running contract code") in
    let* code_trace = trace_code
        ~script:script.expanded
        ~storage:storage.expanded
        ~input:input.expanded
        ~expansion_table:(script.expansion_table |> Array.of_list)
        ~make_logger
        t.alpha_context
    in

    return {t with code_trace=(Some code_trace)}

end
