open Protocol
open Alpha_context
module Plugin = Tezos_protocol_plugin_014_PtKathma
module RPC = Tezos_rpc_http_client_unix.RPC_client_unix (* TODO use lib_mockup one? *)
module Client_context_unix = Tezos_client_base_unix.Client_context_unix
module Client_context = Tezos_client_base.Client_context

(* need to retain the trace of code locs of any michelson errors *)
module StepperExpr = struct
  include Expr

  exception Expression_from_string_with_locs of error trace

  module Michelson_v1_parser = Tezos_client_014_PtKathma.Michelson_v1_parser

  let of_source_exn ?(check=false) script =
    let ast, errs =
      Michelson_v1_parser.parse_toplevel ~check script
    in
    (match errs with
     | [] -> ()
     | lst -> raise @@ Expression_from_string_with_locs lst
    );
    ast

  (** Parse a Michelson expression from string, raising an exception on error,
      in this version we keep hold of the inner errors. *)
  let from_string_exn ?(check_micheline_indentation = false) str =
    let ast, errs =
      Michelson_v1_parser.parse_expression ~check:check_micheline_indentation str
    in
    (match errs with
     | [] -> ()
     | lst -> raise @@ Expression_from_string_with_locs lst
    );
    ast

end

module T (Interp : Mdb_types.INTERPRETER) = struct

  type code_trace = (Script.expr * Apply_internal_results.packed_internal_contents trace * Script_typed_ir.execution_trace * Lazy_storage.diffs option)

  type t = {
    chain_id:Chain_id.t;
    rpc_context:Environment.Updater.rpc_context;
    mock_context:Client_context_unix.unix_mockup;
    code_trace:code_trace;
  }

  let code_trace t = t.code_trace
  let chain_id t = t.chain_id
  let rpc_context t = t.rpc_context
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

  let trace_code
      ?gas
      ?(entrypoint = Entrypoint.default)
      ?balance
      ~script
      ~storage
      ~input
      ?(amount = Tez.fifty_cents)
      ?(chain_id = Chain_id.zero)
      ?source
      ?payer
      ?self
      ?now
      ?level
      ~logger
      ctxt =

    let open Lwt_result_syntax in

    (* NOTE the RPC call to this also has block parameter after ctxt and type of ctxt is
       'pr #Environment.RPC_context.simple where type of block is 'pr *)
    let storage = Script.lazy_expr storage in
    let code = Script.lazy_expr script in
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
    let gas =
      match gas with
      | Some gas -> gas
      | None ->
        Gas.Arith.integral_of_int_exn
          100 (* Constants.hard_gas_limit_per_operation ctxt *)
    in
    let ctxt = Gas.set_limit ctxt gas in
    let now = match now with None -> Script_timestamp.now ctxt | Some t -> t in
    let level =
      match level with
      | None ->
        (Level.current ctxt).level |> Raw_level.to_int32 |> Script_int.of_int32
        |> Script_int.abs
      | Some z -> z
    in
    let step_constants =
      let open Script_interpreter in
      {source; payer; self; amount; balance; chain_id; now; level}
    in

    (* NOTE in the old code this was a Lwt_result.map - so wouldnt need the return() at end of code block *)
    let* res =
      Interp.execute
        ctxt
        step_constants
        ~script:{storage; code}
        ~entrypoint
        ~parameter:input
        ~logger
      |> Lwt.map Environment.wrap_tzresult
    in
    let (
      ( Script_interpreter.{
            script = _;
            code_size = _;
            storage;
            operations;
            lazy_storage_diff;
            ticket_diffs = _;
          }, _ctxt ), trace ) = res
    in
    let ops = Apply_internal_results.contents_of_packed_internal_operations operations in
    return (
      storage,
      ops,
      trace,
      lazy_storage_diff
    )


  let process
      ~logger
      protocol_str
      base_dir
      contract_file =

    let open Lwt_result_syntax in

    Random.self_init ();

    (* believe this RPC config gets overwritten in mock creation, same for the #full cctxt? *)
    Printf.printf "making rpc config\n";
    let rpc_config = RPC.{media_type=Any; endpoint=Uri.empty; logger=null_logger} in
    Printf.printf "making client context unix full\n";
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

    Printf.printf "making client context unix mockup\n";
    let protocol_hash = Protocol_hash.of_b58check_opt protocol_str in
    let* {chain_id; rpc_context; unix_mockup=mock_context} = Mdb_stepper_config.setup_mockup_rpc_client_config
        (cctxt :> Client_context.printer)
        protocol_hash
        base_dir
    in


    let timestamp = rpc_context.block_header.timestamp in
    let level = Int32.succ rpc_context.block_header.level in (* `Successor_level is safer? *)
    let* (alpha_ctxt, _, _) =
      Protocol.Alpha_context.prepare
        ~level
        ~predecessor_timestamp:timestamp
        ~timestamp
        rpc_context.context
      |> Lwt.map Environment.wrap_tzresult
    in

    (* Printf.printf "doing client config init mockup - ONLY NEEDED FOR DISK BASED MOCKS\n"; *)
    (* let* () = Client_config.config_init_mockup *)
    (*     unix_mockup *)
    (*     protocol_hash *)
    (*     bootstrap_accounts_filename *)
    (*     protocol_constants_filename *)
    (*     base_dir *)
    (* in *)


    (* TODO not sure if we need a remote signer in mock mode *)
    (* Printf.printf "setup remote signer with mock client config\n\n"; *)
    (* setup_remote_signer *)
    (*   (module C) *)
    (*   client_config *)
    (*   rpc_config *)
    (*   parsed_config_file ; *)

    Printf.printf "reading contract file\n";
    let* source = mock_context#read_file contract_file in
    let script = StepperExpr.of_source_exn source in
    let mich_unit = StepperExpr.from_string_exn "Unit" in

    let* code_trace = trace_code
        ~script:script.expanded
        ~storage:mich_unit.expanded
        ~input:mich_unit.expanded
        ~logger
        alpha_ctxt
    in

    return {chain_id; rpc_context; mock_context; code_trace}


  let step
      ~logger
      ~protocol_str
      ~base_dir
      contract_file =
    Lwt_preemptive.run_in_main (
      fun () -> process
          ~logger
          protocol_str
          base_dir
          contract_file
    )
end
