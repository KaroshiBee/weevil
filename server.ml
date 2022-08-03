(** Multi-client server example.

    Clients can increment a shared counter or read its current value.

    Build with: ocamlfind ocamlopt -package lwt,lwt.unix,logs,logs.lwt -linkpkg -o counter-server ./counter-server.ml
 *)
module DRq = Dapper.Dap_request
module DRs = Dapper.Dap_response


let backlog = 10

type event =
  | NextReq of DRq.NextRequest.cls_t
  | StackTrace of DRq.StackTraceRequest.cls_t
  | Unknown of string


let handle_message msg =
  let open Data_encoding.Json in
  let parse_next_req js =
    try
      let req = destruct DRq.NextRequest.enc js in
      Some (NextReq req)
    with _ ->
      let _ = Logs_lwt.warn (fun m -> m "Not next request") in
      None
  in
  let parse_stacktrace_req js =
    try
      let req = destruct DRq.StackTraceRequest.enc js in
      Some (StackTrace req)
    with _ ->
      let _ = Logs_lwt.warn (fun m -> m "Not stack trace request") in
      None
  in
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
  | _ -> (
      match from_string msg with
      | Ok js -> (
          match parse_next_req js with
          | Some next_req -> next_req
          | None -> match parse_stacktrace_req js with
            | Some st_req -> st_req
            | None -> Unknown "Unknown js"
        )
      | Error err ->
        let s = Printf.sprintf "Unknown command '%s' - error '%s'" msg err in
        Unknown s
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
                     let _ = Unix.close_process (ic_process, oc_process) in (); Logs_lwt.warn (fun m -> m "Process finished")
                   with Unix.Unix_error _ ->
                     Logs_lwt.warn (fun m -> m "Process finished")
                 )
             in
             let resp = new DRs.NextResponse.cls seq request_seq !success command in
             let resp = construct DRs.NextResponse.enc resp |> to_string |> Defaults._replace "\n" "" in
             Logs_lwt.info (fun m -> m "Next response \n%s\n" resp);
           )
           | StackTrace req ->
             let seq = Int64.succ req#seq in
             let request_seq = req#seq in
             let success = true in
             let command = req#command in
             (* TODO read frames from ic_process *)
             let body = DRs.StackTraceResponse.{stackFrames=[]; totalFrames=None} in
             let resp = new DRs.StackTraceResponse.cls seq request_seq success command body in
             let resp = construct DRs.StackTraceResponse.enc resp |> to_string |> Defaults._replace "\n" "" in
             Logs_lwt.info (fun m -> m "Stack trace response \n%s\n" resp);
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
