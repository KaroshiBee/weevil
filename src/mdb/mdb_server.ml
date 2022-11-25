module Tz = Tezos_base.TzPervasives
open Tz.Error_monad
module Client_config = Tezos_client_base_unix.Client_config
module Client_context = Tezos_client_base.Client_context
module Client_context_unix = Tezos_client_base_unix.Client_context_unix
module Protocol_client_context = Tezos_client_014_PtKathma.Protocol_client_context
module Programs = Tezos_client_014_PtKathma.Client_proto_programs
module RPC = Tezos_rpc_http_client_unix.RPC_client_unix (* TODO use lib_mockup one? *)

module Conduit = Conduit_lwt_unix
module Js = Data_encoding.Json
module Dap = Dapper.Dap
module MichEvent = Mdb_event

let setup_mockup_rpc_client_config
    (cctxt : Client_context.printer)
    ?(protocol_hash : Tz.Protocol_hash.t option)
    base_dir =
  let open Lwt_result_syntax in
  let in_memory_mockup (protocol : Tz.Protocol_hash.t option) =
    match protocol with
    | None -> Tezos_mockup.Persistence.default_mockup_context cctxt
    | Some protocol_hash ->
        Tezos_mockup.Persistence.init_mockup_context_by_protocol_hash
          ~cctxt
          ~protocol_hash
          ~constants_overrides_json:None
          ~bootstrap_accounts_json:None
  in
  let* b = Tezos_mockup.Persistence.classify_base_dir base_dir in
  let* (mockup_env, {chain = chain_id; rpc_context; protocol_data}), mem_only =
    match b with
    | Tezos_mockup.Persistence.Base_dir_is_empty
    | Tezos_mockup.Persistence.Base_dir_is_file
    | Tezos_mockup.Persistence.Base_dir_is_nonempty
    | Tezos_mockup.Persistence.Base_dir_does_not_exist ->
        let mem_only = true in
        let* res = in_memory_mockup protocol_hash in
        return (res, mem_only)
    | Tezos_mockup.Persistence.Base_dir_is_mockup ->
        let mem_only = false in
        let* res =
          Tezos_mockup.Persistence.get_mockup_context_from_disk
            ~base_dir
            ~protocol_hash
            cctxt
        in
        return (res, mem_only)
  in
  return
    (new Client_context_unix.unix_mockup
       ~base_dir
       ~mem_only
       ~mockup_env
       ~chain_id
       ~rpc_context
       ~protocol_data)


(* need to retain the trace of code locs of any michelson errors *)
module StepperExpr = struct
  include Expr

  exception Expression_from_string_with_locs of Tz.error list

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
    contract_file =
  (* TODO do we need Random.init () *)
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
  let* protocol_hash = Lwt.return @@ Tz.Protocol_hash.of_b58check protocol_str in
  let* unix_mockup = setup_mockup_rpc_client_config
      (cctxt :> Client_context.printer)
      ~protocol_hash
      base_dir
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
  let* source = unix_mockup#read_file contract_file in
  let script = StepperExpr.of_source source in
  let mich_unit = StepperExpr.from_string "Unit" in

  let shared_params = Programs.{
    input=mich_unit;
    unparsing_mode=Protocol.Script_ir_translator.Readable;
    now=None;
    level=None;
    source=None;
    payer=None;
    gas=None;
  }
  in

  let run_params = Programs.{
    shared_params;
    amount=None;
    balance=None;
    program=script;
    storage=mich_unit;
    entrypoint=None;
    self=None
  }
  in

  Printf.printf "making protocol client context\n";
  (* this thing can do the call_proto_service0 thing *)
  let cpctxt = (new Protocol_client_context.wrap_full unix_mockup) in

  let* res = Programs.trace
      cpctxt
      ~chain:cpctxt#chain
      ~block:cpctxt#block
      run_params
  in

    (* (cpctxt :> Protocol_client_Context.rpc_context) *)
    (* run_params *)

  return (unix_mockup, cpctxt, res)

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


let rec main_handler ~input_mvar ~stepper_process ic oc =
  let%lwt ln = Lwt_io.read_line_opt ic in
  match ln, stepper_process with
  | Some _msg, None -> (
      (* let fname = "/home/wyn/dev/weevil/src/backend/tests/backend_cram_tests/stepper_test.t/multiply_2_x_250_equals_500.tz" in *)
      (* let p = Lwt_preemptive.detach (fun (headless, filename) -> process ~input_mvar headless (Some filename)) (true, fname) in *)
      (* let%lwt () = Logs_lwt.info (fun m -> m "[MICH] spawned '%s'" fname) in *)
      (* Lwt.join [p; main_handler ~input_mvar ~stepper_process:(Some p) ic oc] *)
      main_handler ~input_mvar ~stepper_process:None ic oc
    )

  | Some _msg, Some p -> (
    let%lwt () = Logs_lwt.info (fun m -> m "[MICH] process already spawned") in
    let%lwt () = Logs_lwt.info (fun m -> m "[MICH] process state %s" @@ match Lwt.state p with | Return _x -> "finished" | Sleep -> "sleep" | Fail _exn -> "failed") in
    match Lwt.state p with
    | Sleep ->
      let p = Lwt_mvar.put input_mvar _msg in
      Lwt.join [p; main_handler ~input_mvar ~stepper_process ic oc]
    | Return _ | Fail _ ->
      main_handler ~input_mvar ~stepper_process:None ic oc
  )

  | None, _ -> Logs_lwt.info (fun m -> m "[MICH] connection closed")

let on_exn exn =
  Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

let on_connection _flow ic oc =
  let%lwt () = Logs_lwt.info (fun m -> m "[MICH] got connection") in
  let input_mvar = Lwt_mvar.create_empty () in
  main_handler ~input_mvar ~stepper_process:None ic oc

let lwt_svc ?stopper port =
  let open Lwt in
  let mode = `TCP (`Port port) in
  let () = Logs.info (fun m -> m "[MICH] starting backend server on port %d" port) in
  let () = Lwt_preemptive.simple_init () in
  (* only allowing one interp process at a time *)
  let () = Lwt_preemptive.set_bounds (1,1) in
  let ret =
    Conduit.init () >>= fun ctx -> (
      match stopper with
      | Some stop -> Conduit.serve ~stop ~on_exn ~ctx ~mode on_connection
      | None -> Conduit.serve ~on_exn ~ctx ~mode on_connection
    )
    >|= fun _ ->
    `Ok ()
  in
  ret

let svc ~port =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  Lwt_main.run (lwt_svc port)
