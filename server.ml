open Conduit_lwt_unix
open Data_encoding.Json
module DRq = Dapper.Dap_request
module DRs = Dapper.Dap_response
module DEv = Dapper.Dap_event
module Db = Dapper.Dap_base

let recs : Model.Weevil_json.t list ref  = ref []

type event =
  | InitReq of DRq.InitializeRequest.cls_t
  | AttachReq of DRq.AttachRequest.cls_t
  | ConfigDoneReq of DRq.ConfigurationDoneRequest.cls_t
  | ThreadsReq of DRq.ThreadsRequest.cls_t
  | ContinueReq of DRq.ContinueRequest.cls_t
  | NextReq of DRq.NextRequest.cls_t
  | StackTrace of DRq.StackTraceRequest.cls_t
  | Scopes of DRq.ScopesRequest.cls_t
  | Variables of DRq.VariablesRequest.cls_t
  | DisconnectReq of DRq.DisconnectRequest.cls_t
  | Unknown of string
  | Empty


(* TODO also want to do scopes and variables
 *  for our example there is only one stack frame
 *  which is the latest stack on the history
 *  there is only one scope which is 'locals',
 *  which contains the 'stack' and 'gas'
 *  and there are multiple variables which are indexed
 *  children of the main michelson stack *)

let parse_initialize_req js =
  try
    let req = destruct DRq.InitializeRequest.enc js in
    assert(req#command = DRq.Request.Initialize);
    Some (InitReq req)
  with _ ->
    let _ = Logs_lwt.debug (fun m -> m "[DAP] Not init request") in
    None

let parse_attach_req js =
  try
    let req = destruct DRq.AttachRequest.enc js in
    assert(req#command = DRq.Request.Attach);
    Some (AttachReq req)
  with _ ->
    let _ = Logs_lwt.debug (fun m -> m "[DAP] Not attach request") in
    None

let parse_configuration_req js =
  try
    let req = destruct DRq.ConfigurationDoneRequest.enc js in
    assert(req#command = DRq.Request.ConfigurationDone);
    Some (ConfigDoneReq req)
  with _ ->
    let _ = Logs_lwt.debug (fun m -> m "[DAP] Not config done request") in
    None

let parse_threads_req js =
  try
    let req = destruct DRq.ThreadsRequest.enc js in
    assert(req#command = DRq.Request.Threads);
    Some (ThreadsReq req)
  with _ ->
    let _ = Logs_lwt.debug (fun m -> m "[DAP] Not threads request") in
    None

let parse_continue_req js =
  try
    let req = destruct DRq.ContinueRequest.enc js in
    assert(req#command = DRq.Request.Continue);
    Some (ContinueReq req)
  with _ ->
    let _ = Logs_lwt.debug (fun m -> m "[DAP] Not continue request") in
    None

let parse_next_req js =
  try
    let req = destruct DRq.NextRequest.enc js in
    assert(req#command = DRq.Request.Next);
    Some (NextReq req)
  with _ ->
    let _ = Logs_lwt.debug (fun m -> m "[DAP] Not next request") in
    None

let parse_stacktrace_req js =
  try
    let req = destruct DRq.StackTraceRequest.enc js in
    assert(req#command = DRq.Request.StackTrace);
    Some (StackTrace req)
  with _ ->
    let _ = Logs_lwt.debug (fun m -> m "[DAP] Not stack trace request") in
    None

let parse_scopes_req js =
  try
    let req = destruct DRq.ScopesRequest.enc js in
    assert(req#command = DRq.Request.Scopes);
    Some (Scopes req)
  with _ ->
    let _ = Logs_lwt.debug (fun m -> m "[DAP] Not scopes request") in
    None

let parse_variables_req js =
  try
    let req = destruct DRq.VariablesRequest.enc js in
    assert(req#command = DRq.Request.Variables);
    Some (Variables req)
  with _ ->
    let _ = Logs_lwt.debug (fun m -> m "[DAP] Not variables request") in
    None

let parse_disconnect_req js =
  try
    let req = destruct DRq.DisconnectRequest.enc js in
    assert(req#command = DRq.Request.Disconnect);
    Some (DisconnectReq req)
  with _ ->
    let _ = Logs_lwt.debug (fun m -> m "[DAP] Not disconnect request") in
    None

let parsers = [
  parse_initialize_req;
  parse_attach_req;
  parse_configuration_req;
  parse_threads_req;
  parse_continue_req;
  parse_next_req;
  parse_stacktrace_req;
  parse_scopes_req;
  parse_variables_req;
  parse_disconnect_req;
]

(* let read_line_ i = try Some (input_line i) with End_of_file -> None
 *
 * let rec lines_from_in_channel i acc =
 *   match (read_line_ i) with
 *   | None -> List.rev acc
 *   | Some s -> lines_from_in_channel i (s :: acc)
 *
 * let read_log ic_process () =
 *   let i = ic_process in (\* open_in Defaults._DEFAULT_LOG_FILE in *\)
 *   try
 *     let lns = lines_from_in_channel i [] in
 *     close_in i;
 *     lns
 *   with _ ->
 *     close_in_noerr i;
 *     []
 *
 * let read_weevil_log ic_process () =
 *   read_log ic_process ()
 *   |> List.filter (fun ln -> 0 < String.length ln && String.get ln 0 != '#')
 *   |> List.filter_map (fun ln ->
 *       from_string ln
 *       |> Result.to_option
 *     )
 *   |> List.map (fun ln ->
 *       destruct Model.Weevil_json.enc ln
 *     ) *)

let handle_message msg =
  (* let msg = if !content_length > 0 then String.sub msg 0 !content_length else msg in *)
  match msg with
  (* NOTE 'n' and 'st' are helpers to quickly test stuff *)
  | "n" -> (
      let args = DRq.NextArguments.{threadId=Defaults._THE_THREAD_ID; singleThread=None; granularity=None} in
      let req = new DRq.NextRequest.cls 1 args in
      NextReq req
    )
  | "st" -> (
      let args = DRq.StackTraceArguments.{threadId=Defaults._THE_THREAD_ID; startFrame=None; levels=None} in
      let req = new DRq.StackTraceRequest.cls 1 args in
      StackTrace req
    )
  | "sc" -> (
      let args = DRq.ScopesArguments.{frameId=Defaults._THE_FRAME_ID} in
      let req = new DRq.ScopesRequest.cls 1 args in
      Scopes req
    )
  | "v" -> (
      let args = DRq.VariablesArguments.{variablesReference=Defaults._THE_ONLY_SCOPE |> snd} in
      let req = new DRq.VariablesRequest.cls 1 args in
      Variables req
    )
  | "\r\n" -> Empty
  | "" -> Empty
  | _ -> (
      (* NOTE when not using helpers,
         the Content-Length header field and other \r\n end up as errors
         *)
      match from_string msg with (* Result.bind (Defaults.strip_header msg) from_string with *)
      | Ok js -> (
          (* NOTE just grab the first event type that can be parsed *)
          match List.filter_map (fun p -> p js) parsers with
          | req :: _ -> req
          | [] -> Unknown (Printf.sprintf "Unknown js: %s" msg)
        )
      | Error err ->
        (* TODO be better *)
        let s = Printf.sprintf "bad command '%s' - error '%s'" msg err in
        Unknown s
    )

let handle_msg oc msg oc_process =
  Logs_lwt.debug (fun m -> m "[DAP] Got msg: '%s'" msg) >>= fun _ ->
  match handle_message msg with
  | InitReq req -> (
      let seq = succ req#seq in
      let request_seq = req#seq in
      let success = ref true in
      let command = req#command in
      let body = Db.Capabilities.make ~supportsTerminateRequest:true ~supportsConfigurationDoneRequest:true () in
      let resp = new DRs.InitializeResponse.cls seq request_seq !success command body in
      let resp = construct DRs.InitializeResponse.enc resp |> Defaults.wrap_header in
      (* send init event too *)
      let seq = succ seq in
      let ev = new DEv.InitializedEvent.cls seq in
      let ev = construct DEv.InitializedEvent.enc ev |> Defaults.wrap_header in
      Logs_lwt.debug (fun m -> m "[DAP] \nInit response \n%s\n" resp) >>= fun _ ->
      Logs_lwt.debug (fun m -> m "[DAP] \nInit event \n%s\n" ev) >>= fun _ ->
      Lwt_io.write oc resp >>= fun _ ->
      Lwt_io.write oc ev
    )
  | AttachReq req -> (
      (* TODO should really spin up stepper here? *)
      let seq = succ req#seq in
      let request_seq = req#seq in
      let success = ref true in
      let command = req#command in
      let resp = new DRs.AttachResponse.cls seq request_seq !success command None in
      let resp = construct DRs.AttachResponse.enc resp |> Defaults.wrap_header in
      Logs_lwt.debug (fun m -> m "[DAP] \nAttach response \n%s\n" resp) >>= fun _ ->
      Lwt_io.write oc resp
    )
  | ConfigDoneReq req -> (
      let seq = succ req#seq in
      let request_seq = req#seq in
      let success = ref true in
      let command = req#command in
      let resp = new DRs.ConfigurationDoneResponse.cls seq request_seq !success command None in
      let resp = construct DRs.ConfigurationDoneResponse.enc resp |> Defaults.wrap_header in
      let body = DEv.StoppedEvent.(body ~reason:Entry ~threadId:Defaults._THE_THREAD_ID ()) in
      let seq = succ seq in
      let ev = new DEv.StoppedEvent.cls seq body in
      let ev = construct DEv.StoppedEvent.enc ev |> Defaults.wrap_header in
      Logs_lwt.debug (fun m -> m "[DAP] \nConfigurationDone response \n%s\n" resp) >>= fun _ ->
      Logs_lwt.debug (fun m -> m "[DAP] \nStopped event \n%s\n" ev) >>= fun _ ->
      Lwt_io.write oc resp >>= fun _ ->
      Lwt_io.write oc ev
    )
  | ContinueReq req -> (
      let seq = succ req#seq in
      let request_seq = req#seq in
      let success = ref true in
      let command = req#command in
      let _ =
        try
          Lwt_io.write oc_process "step\n" >>= fun _ ->
          Logs_lwt.debug (fun m -> m "[DAP] Got Continue request\n%s\n" msg)
        with Sys_error _ -> (
            success := false;
            (* run out of contract to step through *)
            try
              (* let _ = Unix.close_process (ic_process, oc_process) in (); *)
              Logs_lwt.warn (fun m -> m "[DAP] Process finished: sys error")
            with Unix.Unix_error _ ->
              Logs_lwt.warn (fun m -> m "[DAP] Process finished: unix error")
          )
      in
      let body = DRs.ContinueResponse.{allThreadsContinued=(Some !success)} in
      let resp = new DRs.ContinueResponse.cls seq request_seq !success command body in
      let resp = construct DRs.ContinueResponse.enc resp |> Defaults.wrap_header in
      let body = DEv.StoppedEvent.(body ~reason:Step ~threadId:Defaults._THE_THREAD_ID ()) in
      let seq = succ seq in
      let ev = new DEv.StoppedEvent.cls seq body in
      let ev = construct DEv.StoppedEvent.enc ev |> Defaults.wrap_header in
      Logs_lwt.debug (fun m -> m "[DAP] \nContinue response \n%s\n" resp) >>= fun _ ->
      Logs_lwt.debug (fun m -> m "[DAP] \nStopped event \n%s\n" ev) >>= fun _ ->
      Lwt_io.write oc resp >>= fun _ ->
      Lwt_io.write oc ev
    )
  | ThreadsReq req -> (
      let seq = succ req#seq in
      let request_seq = req#seq in
      let success = true in
      let command = req#command in
      let id = Defaults._THE_THREAD_ID in
      let threads = [
        Db.Thread.{id; name="main"};
      ]
      in
      let body = DRs.ThreadsResponse.{threads} in
      let resp = new DRs.ThreadsResponse.cls seq request_seq success command body in
      let resp = construct DRs.ThreadsResponse.enc resp |> Defaults.wrap_header in
      Logs_lwt.debug (fun m -> m "[DAP] Got threads request\n%s\n" msg) >>= fun _ ->
      Logs_lwt.debug (fun m -> m "[DAP] Threads response \n%s\n" resp) >>=
      (fun _ -> Lwt_io.write oc resp)
    )
  | NextReq req -> (
      let seq = succ req#seq in
      let request_seq = req#seq in
      let success = ref true in
      let command = req#command in
      let _ =
        try
          Lwt_io.write oc_process "step\n" >>= fun _ ->
          Logs_lwt.debug (fun m -> m "[DAP] Got Next request\n%s\n" msg)
        with Sys_error _ -> (
            success := false;
            (* run out of contract to step through *)
            try
              (* let _ = Unix.close_process (ic_process, oc_process) in (); *)
              Logs_lwt.warn (fun m -> m "[DAP] Process finished: sys error")
            with Unix.Unix_error _ ->
              Logs_lwt.warn (fun m -> m "[DAP] Process finished: unix error")
          )
      in
      (* send the response *)
      let resp = new DRs.NextResponse.cls seq request_seq !success command None in
      let resp = construct DRs.NextResponse.enc resp |> Defaults.wrap_header in
      (* send stopped event too *)
      let seq = succ seq in
      let body = DEv.StoppedEvent.(body ~reason:Step ~threadId:Defaults._THE_THREAD_ID()) in
      let ev = new DEv.StoppedEvent.cls seq body in
      let ev = construct DEv.StoppedEvent.enc ev |> Defaults.wrap_header in
      Logs_lwt.debug (fun m -> m "[DAP] \nNext response \n%s\nStopped event\n%s\n" resp ev) >>=
      (fun _ -> Lwt_io.write oc resp) >>=
      (fun _ -> Lwt_io.write oc ev)
    )
  | StackTrace req -> (
      let seq = succ req#seq in
      let request_seq = req#seq in
      let success = true in
      let command = req#command in
      (* TODO should be able to arrange so that can read frames from ic_process? *)
      let stackFrames =
        match !recs |> List.rev |> List.hd with
        | None -> []
        | Some wrec ->
          let loc = Model.Weevil_json.relative_loc wrec in
          let source = Some (Db.Source.make ~name:"example.tz" ~path:"/home/wyn/dev/weevil/example.tz" ()) in
          [Db.StackFrame.{id=Defaults._THE_FRAME_ID; name=Defaults._THE_FRAME_NAME; source; line=loc; column=0}]
      in
      let totalFrames = Some (List.length stackFrames) in
      let body = DRs.StackTraceResponse.{stackFrames; totalFrames} in
      let resp = new DRs.StackTraceResponse.cls seq request_seq success command body in
      let resp = construct DRs.StackTraceResponse.enc resp |> Defaults.wrap_header in
      Logs_lwt.info (fun m -> m "[DAP] Got StackTrace request\n%s\n" msg) >>= fun _ ->
      Logs_lwt.info (fun m -> m "[DAP] Stack trace response \n%s\n" resp) >>=
      (fun _ -> Lwt_io.write oc resp)
    )
  | Scopes req -> (
      (* TODO would need to pull the frameId from req and look up the scopes from that *)
      let seq = succ req#seq in
      let request_seq = req#seq in
      let success = true in
      let command = req#command in
      let locals_name, locals_var = Defaults._THE_ONLY_SCOPE in
      (* let _gas_name, gas_var = Defaults._THE_GAS_LOCAL in
       * let _stack_name, stack_var = Defaults._THE_MICHELSON_STACK_LOCAL in *)
      let scopes = [
        Db.Scope.{name=locals_name; variablesReference=locals_var; expensive=false};
      ]
      in
      let body = DRs.ScopesResponse.{scopes} in
      let resp = new DRs.ScopesResponse.cls seq request_seq success command body in
      let resp = construct DRs.ScopesResponse.enc resp |> Defaults.wrap_header in
      Logs_lwt.debug (fun m -> m "[DAP] Got Scopes request\n%s\n" msg) >>= fun _ ->
      Logs_lwt.debug (fun m -> m "[DAP] Scopes response \n%s\n" resp) >>=
      (fun _ -> Lwt_io.write oc resp)
    )
  | Variables req -> (
      let seq = succ req#seq in
      let request_seq = req#seq in
      let success = true in
      let command = req#command in
      let _vref = req#arguments.variablesReference in
      let gas_name, _gas_var = Defaults._THE_GAS_LOCAL in
      let stack_name, _stack_var = Defaults._THE_MICHELSON_STACK_LOCAL in
      let gas_val, stack_val =
        match !recs |> List.rev |> List.hd with
        | None -> "", ""
        | Some wrec -> Model.Weevil_json.(wrec.gas, String.concat ", " wrec.stack |> Printf.sprintf "[%s]")
      in
      let variables = [
        Db.Variable_.{name=gas_name; value=gas_val; variablesReference=0}; (* 0 here means not structured ie no children? *)
        Db.Variable_.{name=stack_name; value=stack_val; variablesReference=0}; (* 0 here means not structured ie no children? *)
      ]
      in
      let body = DRs.VariablesResponse.{variables} in
      let resp = new DRs.VariablesResponse.cls seq request_seq success command body in
      let resp = construct DRs.VariablesResponse.enc resp |> Defaults.wrap_header in
      Logs_lwt.debug (fun m -> m "[DAP] Got Variables request\n%s\n" msg) >>= fun _ ->
      Logs_lwt.debug (fun m -> m "[DAP] Variables response \n%s\n" resp) >>=
      (fun _ -> Lwt_io.write oc resp)
    )
  | DisconnectReq req -> (
      let seq = succ req#seq in
      let request_seq = req#seq in
      let success = true in
      let command = req#command in
      let resp = new DRs.DisconnectResponse.cls seq request_seq success command None in
      let resp = construct DRs.DisconnectResponse.enc resp |> Defaults.wrap_header in
      Logs_lwt.debug (fun m -> m "[DAP] Got Disconnect request\n%s\n" msg) >>= fun _ ->
      Logs_lwt.debug (fun m -> m "[DAP] Disconnect response \n%s\n" resp) >>= fun _ ->
      Lwt_io.write oc resp >>= fun _ ->
      let seq = succ seq in
      let body = DEv.TerminatedEvent.{restart=None} in
      let ev = new DEv.TerminatedEvent.cls seq body in
      let ev = construct DEv.TerminatedEvent.enc ev |> Defaults.wrap_header in
      Logs_lwt.debug (fun m -> m "[DAP] Terminated Event \n%s\n" ev) >>= fun _ ->
      Lwt_io.write oc ev
    )
  | Unknown s -> Logs_lwt.warn (fun m -> m "[DAP] Unknown '%s'" s)
  | Empty -> Logs_lwt.debug (fun m -> m "[DAP] Empty")


let on_exn exn = Lwt.ignore_result @@ Logs_lwt.err (fun m -> m "%s" @@ Printexc.to_string exn)

type content_length = int option

let rec dap_handler ~content_length ~oc_process _flow ic oc =
  match content_length with
  | Some count ->
      Logs_lwt.info (fun m -> m "[DAP] got count %d" count) >>= fun _ ->
      (* \r\n throw away *)
      Lwt_io.read ~count:2 ic >>= fun header_break ->
      assert (header_break = "\r\n") |> Lwt.return >>= fun _ ->
      Lwt_io.read ~count ic >>= fun msg ->
      Logs_lwt.info (fun m -> m "[DAP] Got message '%s'" msg) >>= fun _ ->
      handle_msg oc msg oc_process >>= fun _ ->
      dap_handler ~content_length:None ~oc_process _flow ic oc
  | None -> (
      Logs_lwt.info (fun m -> m "[DAP] no content length yet") >>= fun _ ->
      Lwt_io.read_line_opt ic >>= function
      | Some msg ->
          let rgx = Str.regexp_string Defaults._HEADER_FIELD in
          let content_length =
            if Defaults.(contains msg _HEADER_FIELD) then
              match Str.split rgx msg with
              | [n] ->
                  let i = int_of_string n in
                  Some i
              | _ -> None
            else None
          in
          dap_handler ~content_length ~oc_process _flow ic oc
      | None -> Logs_lwt.info (fun m -> m "[DAP] connection closed"))

let read_weevil_log ln =
  if 0 < String.length ln && String.get ln 0 != '#' then (
    match from_string ln with
    | Ok ln ->
      let ln = destruct Model.Weevil_json.enc ln in
      Some ln
    | Error e ->
      Logs.warn (fun m -> m "Cannot decode '%s': %s" ln e);
      None
  ) else None


let rec main_handler ~mode ~content_length (process_full:Lwt_process.process_full) =
  let p = process_full in
  let dap_svc =
    init () >>= fun ctx ->
    serve ~on_exn ~ctx ~mode (dap_handler ~content_length ~oc_process:p#stdin)
  in

  let step_svc =
    Lwt_io.read_line_opt p#stdout >>= function
    | Some msg ->
      Logs_lwt.info (fun m -> m "[STEPPER] got msg from subprocess '%s'" msg) >>= fun _ ->
      let _ = match read_weevil_log msg with
        | Some wrec ->
          recs := wrec :: !recs;
          Logs_lwt.info (fun m -> m "[STEPPER] got weevil log record from subprocess '%s', %d" msg (List.length !recs))
      | None -> Lwt.return_unit in
      main_handler ~mode ~content_length p
    | None ->
      Logs_lwt.info (fun m -> m "[STEPPER] subprocess complete")
  in
  Lwt.join [dap_svc; step_svc]


let svc ~listen_address ~port ~cmd =
  let _c = cmd in
  let () = assert (listen_address = Unix.inet_addr_loopback) in
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let mode = `TCP (`Port port) in
  let content_length = None in
  let cmd = ("", [|"dune"; "exec"; "--"; "./main.exe"; "stepper"|]) in
  let the_svc = Lwt_process.with_process_full cmd (main_handler ~mode ~content_length) >|= fun _ ->
    `Ok ()
  in

  Lwt_main.run the_svc
