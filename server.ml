(** Multi-client server example.

    Clients can increment a shared counter or read its current value.

    Build with: ocamlfind ocamlopt -package lwt,lwt.unix,logs,logs.lwt -linkpkg -o counter-server ./counter-server.ml
 *)
open Data_encoding.Json
module DRq = Dapper.Dap_request
module DRs = Dapper.Dap_response
module DEv = Dapper.Dap_event
module Db = Dapper.Dap_base

let backlog = 10

type event =
  | NextReq of DRq.NextRequest.cls_t
  | StackTrace of DRq.StackTraceRequest.cls_t
  | Scopes of DRq.ScopesRequest.cls_t
  | Unknown of string

(* TODO also want to do scopes and variables
 *  for our example there is only one stack frame
 *  which is the latest stack on the history
 *  there is only one scope which is 'locals',
 *  which contains the 'stack' and 'gas'
 *  and there are multiple variables which are indexed
 *  children of the main michelson stack *)

let parse_next_req js =
  try
    let req = destruct DRq.NextRequest.enc js in
    Some (NextReq req)
  with _ ->
    let _ = Logs_lwt.warn (fun m -> m "Not next request") in
    None

let parse_stacktrace_req js =
  try
    let req = destruct DRq.StackTraceRequest.enc js in
    Some (StackTrace req)
  with _ ->
    let _ = Logs_lwt.warn (fun m -> m "Not stack trace request") in
    None

let parse_scopes_req js =
  try
    let req = destruct DRq.ScopesRequest.enc js in
    Some (Scopes req)
  with _ ->
    let _ = Logs_lwt.warn (fun m -> m "Not scopes request") in
    None

let parsers = [
  parse_next_req;
  parse_stacktrace_req;
  parse_scopes_req;
]

let handle_message msg =
  match msg with
  (* NOTE 'n' and 'st' are helpers to quickly test stuff *)
  | "n" -> (
      let args = DRq.NextArguments.{threadId=1L; singleThread=None; granularity=None} in
      let req = new DRq.NextRequest.cls 1L args in
      NextReq req
    )
  | "st" -> (
      let args = DRq.StackTraceArguments.{threadId=1L; startFrame=None; levels=None} in
      let req = new DRq.StackTraceRequest.cls 1L args in
      StackTrace req
    )
  | "sc" -> (
      let args = DRq.ScopesArguments.{frameId=Defaults._THE_FRAME_ID} in
      let req = new DRq.ScopesRequest.cls 1L args in
      Scopes req
    )
  | _ -> (
      (* NOTE when not using helpers,
         need to strip off Content-Length header field
         and get inner object *)
      match Result.bind (Defaults.strip_header msg) from_string with
      | Ok js -> (
          (* NOTE just grab the first event type that can be parsed *)
          match List.filter_map (fun p -> p js) parsers with
          | req :: _ -> req
          | [] -> Unknown (Printf.sprintf "Unknown js: %s" msg)
        )
      | Error err ->
        let s = Printf.sprintf "Unknown command '%s' - error '%s'" msg err in
        Unknown s
    )

let read_line_ i = try Some (input_line i) with End_of_file -> None

let rec lines_from_in_channel i acc =
  match (read_line_ i) with
  | None -> List.rev acc
  | Some s -> lines_from_in_channel i (s :: acc)

let read_log () =
  let i = open_in Defaults._DEFAULT_LOG_FILE in
  try
    let lns = lines_from_in_channel i [] in
    close_in i;
    lns
  with _ ->
    close_in_noerr i;
    []

let read_weevil_log () =
  read_log ()
  |> List.filter (fun ln -> 0 < String.length ln && String.get ln 0 != '#')
  |> List.filter_map (fun ln ->
      Data_encoding.Json.from_string ln
      |> Result.to_option
    )
  |> List.map (fun ln ->
      Data_encoding.Json.destruct Model.Weevil_json.enc ln
    )
  |> List.map (fun wrec ->
      let loc = Model.Weevil_json.(wrec.location) in
      Db.StackFrame.{id=Defaults._THE_FRAME_ID; name=Defaults._THE_FRAME_NAME; line=loc; column=0L}
    )

let rec handle_connection ic oc ic_process oc_process () =
  let open Data_encoding.Json in
  Lwt_io.read_line_opt ic >>=
  (fun msg ->
     match msg with
     | Some msg -> (
         let next =
           match handle_message msg with
           | NextReq req -> (
               let seq = Int64.succ req#seq in
               let request_seq = req#seq in
               let success = ref true in
               let command = req#command in
               let _ =
                 try
                   Printf.fprintf oc_process "step\n"; flush oc_process; Logs_lwt.info (fun m -> m "stepping")
                 with Sys_error _ -> (
                     success := false;
                     (* run out of contract to step through *)
                     try
                       let _ = Unix.close_process (ic_process, oc_process) in (); Logs_lwt.warn (fun m -> m "Process finished: sys error")
                     with Unix.Unix_error _ ->
                       Logs_lwt.warn (fun m -> m "Process finished: unix error")
                   )
               in
               (* send the response *)
               let resp = new DRs.NextResponse.cls seq request_seq !success command in
               let resp = construct DRs.NextResponse.enc resp |> Defaults.wrap_header in
               (* send stopped event too *)
               let seq = Int64.succ seq in
               let body = DEv.StoppedEvent.(body ~reason:Step ()) in
               let ev = new DEv.StoppedEvent.cls seq body in
               let ev = construct DEv.StoppedEvent.enc ev |> Defaults.wrap_header in
               Logs_lwt.info (fun m -> m "Next response \n%s\nStopped event\n%s\n" resp ev);
             )
           | StackTrace req -> (
               let seq = Int64.succ req#seq in
               let request_seq = req#seq in
               let success = true in
               let command = req#command in
               (* TODO should be able to arrange so that can read frames from ic_process? *)
               let stackFrames =
                 match read_weevil_log () |> List.rev |> List.hd with
                 | None -> []
                 | Some last_ln -> [last_ln]
               in
               let totalFrames = Some (List.length stackFrames |> Int64.of_int) in
               let body = DRs.StackTraceResponse.{stackFrames; totalFrames} in
               let resp = new DRs.StackTraceResponse.cls seq request_seq success command body in
               let resp = construct DRs.StackTraceResponse.enc resp |> Defaults.wrap_header in
               Logs_lwt.info (fun m -> m "Stack trace response \n%s\n" resp);
             )
           | Scopes req -> (
               (* TODO would need to pull the frameId from req and look up the scopes from that *)
               let seq = Int64.succ req#seq in
               let request_seq = req#seq in
               let success = true in
               let command = req#command in
               let locals_name, locals_var = Defaults._THE_ONLY_SCOPE in
               (* let gas_name, gas_var = Defaults._THE_GAS_LOCAL in
                * let stack_name, stack_var = Defaults._THE_MICHELSON_STACK_LOCAL in *)
               let scopes = [
                 Db.Scope.{name=locals_name; variablesReference=locals_var; expensive=false};
               ]
               in
               let body = DRs.ScopesResponse.{scopes} in
               let resp = new DRs.ScopesResponse.cls seq request_seq success command body in
               let resp = construct DRs.ScopesResponse.enc resp |> Defaults.wrap_header in
               Logs_lwt.info (fun m -> m "Scopes response \n%s\n" resp);
             )
           | Unknown s -> Logs_lwt.warn (fun m -> m "Unknown '%s'" s)

         in
         next >>= handle_connection ic oc ic_process oc_process
     )
     | None -> Logs_lwt.info (fun m -> m "Connection closed"))

let accept_connection cmd conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  let ic_process, oc_process = Unix.open_process cmd in
  let jobs = Lwt.join [handle_connection ic oc ic_process oc_process ()] in
  Lwt.on_failure (jobs >>= return) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
  Logs_lwt.info (fun m -> m "New connection\n") >>= Lwt.return

let create_socket listen_address port () =
  let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  let _a = Lwt_unix.bind sock @@ ADDR_INET(listen_address, port) in ();
  Lwt_unix.listen sock backlog;
  sock

let create_server cmd sock =
  let rec serve () =
    Lwt_unix.accept sock >>=
    accept_connection cmd >>=
    serve
  in serve

let svc ~listen_address ~port ~cmd () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let sock = create_socket listen_address port () in
  let server = create_server cmd sock () in
  Lwt_main.run @@ server
