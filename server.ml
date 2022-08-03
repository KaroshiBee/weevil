(** Multi-client server example.

    Clients can increment a shared counter or read its current value.

    Build with: ocamlfind ocamlopt -package lwt,lwt.unix,logs,logs.lwt -linkpkg -o counter-server ./counter-server.ml
 *)

(* Shared mutable counter *)
let counter = ref 0

let backlog = 10

type event =
  | Counter of string
  (* | Inc of string *)
  | Step of int
  | Unknown of string

let handle_message msg =
  match msg with
  | "read" -> Counter (string_of_int !counter)
  (* | "inc"  -> counter := !counter + 1; Inc ("Counter has been incremented") *)
  | "n"    -> counter := !counter + 1; Step !counter
  | _      -> Unknown ("Unknown command")

let rec handle_connection ic oc ic_process oc_process () =
  Lwt_io.read_line_opt ic >>=
  (fun msg ->
     match msg with
     | Some msg -> (
         let next =
           match handle_message msg with
           | Step n -> (
             (* WANT TO WRITE TO PROCESS STDIN/OUT  *)
             let open Dapper.Dap_request in
             let args = NextArguments.{threadId=1L; singleThread=None; granularity=None} in
             let rq = new NextRequest.cls (Int64.of_int n) args in
             let request = Data_encoding.Json.(construct NextRequest.enc rq |> to_string |> Defaults._replace "\n" "" ) in
             try
               Printf.fprintf oc_process "%s\n" request; flush oc_process; Logs_lwt.info (fun m -> m "Stepping with \n%s\n" request)
             with Sys_error _ ->
               (* run out of contract to step through *)
               try
                 let _ = Unix.close_process (ic_process, oc_process) in (); Logs_lwt.warn (fun m -> m "Process finished")
               with Unix.Unix_error _ ->
                 Logs_lwt.warn (fun m -> m "Process finished")
           )

           | Counter s | Unknown s ->
             Lwt_io.write_line oc s
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
  Logs_lwt.info (fun m -> m "New connection") >>= Lwt.return

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
