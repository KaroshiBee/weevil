open Protocol
open Alpha_context
open Environment.Error_monad
module Plugin = Tezos_protocol_plugin_014_PtKathma
module RPC = Tezos_rpc_http_client_unix.RPC_client_unix (* TODO use lib_mockup one? *)
module Client_context_unix = Tezos_client_base_unix.Client_context_unix
module Client_context = Tezos_client_base.Client_context

let _originate_dummy_contract ctxt script balance =
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

let _configure_contracts ctxt script balance ~src_opt ~pay_opt ~self_opt =
  (match self_opt with
  | None ->
      let balance =
        Option.value ~default:Plugin.RPC.Scripts.default_balance balance
      in
      _originate_dummy_contract ctxt script balance >>=? fun (ctxt, addr) ->
      return (ctxt, addr, balance)
  | Some addr ->
      Plugin.RPC.Scripts.default_from_context
        ctxt
        (fun c -> Contract.get_balance c @@ Contract.Originated addr)
        balance
      >>=? fun bal -> return (ctxt, addr, bal))
  >>=? fun (ctxt, self, balance) ->
  let (source, payer) =
    match (src_opt, pay_opt) with
    | (None, None) ->
        let self = Contract.Originated self in
        (self, self)
    | (Some c, None) | (None, Some c) -> (c, c)
    | (Some src, Some pay) -> (src, pay)
  in
  return (ctxt, {Plugin.RPC.Scripts.balance; self; source; payer})

(* ?unparsing_mode:Script_ir_translator.unparsing_mode -> *)
(* ?gas:Gas.Arith.integral -> *)
(* ?entrypoint:Entrypoint_repr.t -> *)
(* ?balance:Tez.t -> *)
(* script:Script.expr -> *)
(* storage:Script.expr -> *)
(* input:Script.expr -> *)
(* amount:Tez.t -> *)
(* chain_id:Environment.Chain_id.t -> *)
(* source:Contract.t option -> *)
(* payer:Contract.t option -> *)
(* self:Contract_hash.t option -> *)
(* now:Script_timestamp.t option -> *)
(* level:Script_int.n Script_int.num option -> *)
(* input_mvar:string Lwt_mvar.t -> *)
(* t -> *)
(* (Script.expr * Apply_internal_results.packed_internal_contents list * *)
(*  Script_typed_ir.execution_trace * Lazy_storage.diffs option, error trace) *)
(* result Lwt.t *)
let trace_code
    ?unparsing_mode
    ?gas
    ?(entrypoint = Entrypoint.default)
    ?balance
    ~script
    ~storage
    ~input
    ?(amount = Tez.fifty_cents)
    ?(chain_id = Tezos_base.TzPervasives.Chain_id.zero)
    ?source
    ?payer
    ?self
    ?now
    ?level
    ~input_mvar
    ctxt =
  (* NOTE the RPC call to this also has block parameter after ctxt and type of ctxt is
     'pr #Environment.RPC_context.simple where type of block is 'pr *)
  (* NOTE type unparsing_mode = Optimized | Readable | Optimized_legacy, could we phantom this into the logger type? *)
  let unparsing_mode =
    Option.value ~default:Script_ir_translator.Readable unparsing_mode
  in
  let storage = Script.lazy_expr storage in
  let code = Script.lazy_expr script in
  _configure_contracts
    ctxt
    {storage; code}
    balance
    ~src_opt:source
    ~pay_opt:payer
    ~self_opt:self
  >>=? fun (ctxt, {self; source; payer; balance}) ->
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
  let module Unparsing_mode = struct
    let unparsing_mode = unparsing_mode
  end in
  let module Interp = Mdb_traced_interpreter.T (Unparsing_mode) in
  let logger = Interp.trace_logger ~input_mvar () in
  Interp.execute
    ctxt
    step_constants
    ~script:{storage; code}
    ~entrypoint
    ~parameter:input
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

(* need to retain the trace of code locs of any michelson errors *)
module StepperExpr = struct
  include Expr

  exception Expression_from_string_with_locs of Tezos_error_monad.Error_monad.error list

  module Michelson_v1_parser = Tezos_client_014_PtKathma.Michelson_v1_parser

  let of_source ?(check=false) script =
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
  let from_string ?(check_micheline_indentation = false) str =
    let ast, errs =
      Michelson_v1_parser.parse_expression ~check:check_micheline_indentation str
    in
    (match errs with
     | [] -> ()
     | lst -> raise @@ Expression_from_string_with_locs lst
    );
    ast

end


let process
    ?(protocol_str="PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg")
    ?(base_dir="/tmp/.weevil")
    ?input_mvar:_
    ?(_headless=true)
    _contract_file =

  Random.self_init ();

  let open Lwt_result_syntax in
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
  let protocol_hash = Environment.Protocol_hash.of_b58check_opt protocol_str in
  let* (chain_id, rpc_context, unix_mockup) = Mdb_stepper_config.setup_mockup_rpc_client_config
      (cctxt :> Client_context.printer)
      ?protocol_hash
      base_dir
  in
  return (chain_id, rpc_context, unix_mockup)

  (* let timestamp = rpc_context.block_header.timestamp in *)
  (* let level = Int32.succ rpc_context.block_header.level in (\* `Successor_level is safer? *\) *)
  (* let a = Protocol.Alpha_context.prepare *)
  (*   ~level *)
  (*   ~predecessor_timestamp:timestamp *)
  (*   ~timestamp *)
  (*   rpc_context.context *)
  (* |> Lwt_result.map (fun (actxt, _, _) -> actxt) *)

  (* in *)

  (* (\* Printf.printf "doing client config init mockup - ONLY NEEDED FOR DISK BASED MOCKS\n"; *\) *)
  (* (\* let* () = Client_config.config_init_mockup *\) *)
  (* (\*     unix_mockup *\) *)
  (* (\*     protocol_hash *\) *)
  (* (\*     bootstrap_accounts_filename *\) *)
  (* (\*     protocol_constants_filename *\) *)
  (* (\*     base_dir *\) *)
  (* (\* in *\) *)


  (* (\* TODO not sure if we need a remote signer in mock mode *\) *)
  (* (\* Printf.printf "setup remote signer with mock client config\n\n"; *\) *)
  (* (\* setup_remote_signer *\) *)
  (* (\*   (module C) *\) *)
  (* (\*   client_config *\) *)
  (* (\*   rpc_config *\) *)
  (* (\*   parsed_config_file ; *\) *)

  (* Printf.printf "reading contract file\n"; *)
  (* let* source = unix_mockup#read_file contract_file in *)
  (* let script = StepperExpr.of_source source in *)
  (* let mich_unit = StepperExpr.from_string "Unit" in *)

  (* let* res_ = Mdb_stepper2.trace_code *)
  (*   ~script:script.expanded *)
  (*   ~storage:mich_unit.expanded *)
  (*   ~input:mich_unit.expanded *)
  (*   ~input_mvar:(Lwt_mvar.create_empty ()) *)
  (*   alpha_ctxt *)
  (* in *)

  (* let shared_params = Programs.{ *)
  (*   input=mich_unit; *)
  (*   unparsing_mode=Protocol.Script_ir_translator.Readable; *)
  (*   now=None; *)
  (*   level=None; *)
  (*   source=None; *)
  (*   payer=None; *)
  (*   gas=None; *)
  (* } *)
  (* in *)

  (* let run_params = Programs.{ *)
  (*   shared_params; *)
  (*   amount=None; *)
  (*   balance=None; *)
  (*   program=script; *)
  (*   storage=mich_unit; *)
  (*   entrypoint=None; *)
  (*   self=None *)
  (* } *)
  (* in *)

  (* (\* Printf.printf "making protocol client context\n"; *\) *)
  (* (\* (\\* this thing can do the call_proto_service0 thing *\\) *\) *)
  (* (\* let cpctxt = (new Protocol_client_context.wrap_full unix_mockup) in *\) *)
  (* (\* (\\* ok so there is a bit of wrangling to turn this into an Alpha context, *\) *)
  (* (\*    this gets done on registration with the Resto directory services, *\) *)
  (* (\*    I think we need to go: *\) *)
  (* (\*    cpctxt:full -> Updater.rpc_context = {context:Context.t; _ stuff} -> Services_registration.rpc_context = {context:Alpha_context.t; _ stuff} *\) *)
  (* (\*    then with the alpha context we can call trace_code directly *\) *)
  (* (\* *\\) *\) *)



  (* (\* let* res = Programs.trace *\) *)
  (* (\*     cpctxt *\) *)
  (* (\*     ~chain:cpctxt#chain *\) *)
  (* (\*     ~block:cpctxt#block *\) *)
  (* (\*     run_params *\) *)
  (* (\* in *\) *)

  (* return (chain_id, rpc_context, unix_mockup, res_) *)
(*   let stepper = *)
(*     (\* step but catch any unhandled exceptions and convert to Tz.Error_monad traces *\) *)
(*     let res = *)
(*       try%lwt *)
(*         Mdb_stepper2.trace_code *)
(*           ~script *)
(*           ~storage:mich_unit *)
(*           ~input:mich_unit *)
(*           ~input_mvar *)
(*           cpctxt *)
(*       with *)
(*       | StepperExpr.Expression_from_string_with_locs errs -> Lwt.return @@ Error errs *)
(*       | e -> Lwt.return @@ Error [Tz.error_of_exn e] *)
(*     in *)

(*     (\* convert Tz.Error_monad result to cmdline output, if headless then convert errors to json *\) *)
(*     match%lwt res with *)
(*     | Ok _ -> (\* TODO dont ignore OK output *\) Lwt.return_unit *)
(*     | Error errs as e -> *)
(*       let ss = *)
(*         Format.asprintf "Stepper error - %a" Tz.Error_monad.pp_print_trace errs *)
(*       in *)
(*       let () = *)
(*         if headless then *)
(*           let enc = Tz.Error_monad.result_encoding Tz.Data_encoding.unit in *)
(*           let err_msg = Tz.Data_encoding.Json.(construct enc e |> to_string) |> Dapper.Dap.Header.wrap in *)
(*           Printf.fprintf stderr "%s" err_msg; *)
(*         else () *)
(*       in *)
(*       (\* if in headless mode then dont show --help if the cli has errors *\) *)
(*       raise @@ Sys_error ss *)
(*   in *)
(*   Lwt_preemptive.run_in_main (fun () -> stepper) *)
