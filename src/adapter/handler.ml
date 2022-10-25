open Dapper.Dap_handler_t
module Dap_commands = Dapper.Dap_commands
module Js_msg = Dapper.Dap_js_msg
module Dap_header = Dapper.Dap_header

module type MAKE_HANDLER = sig
  type input
  type output
  type t
  val make : t
  val handle : t -> config:config -> string -> string Lwt.t
end

module MakeHandler (H:HANDLER) : (MAKE_HANDLER with type input := H.input and type output := H.output) = struct

  type t = {
    from_string : string -> H.input;
    to_string : H.output -> (string, string) Result.t Lwt.t ;
    handle : config:config -> H.input -> H.output Lwt.t;
  }

  let make = {
    from_string = H.from_string;
    to_string = H.to_string;
    handle = H.handle;
  }

  let handle t ~config s =
    let%lwt output =
      t.from_string s
      |> t.handle ~config
    in
    match%lwt t.to_string output with
    | Result.Ok msg ->
      Lwt.return msg
    | Result.Error err ->
      Lwt.return err


end

type t = (string, (module HANDLER)) Hashtbl.t

let make : t = [
  "cancel", (module Cancel : HANDLER);
  "initialize", (module Initialize : HANDLER);
  "configurationDone", (module Configuration : HANDLER);
  "launch", (module Launch : HANDLER);
  "attach", (module Attach : HANDLER);
  "restart", (module Restart : HANDLER);
  "disconnect", (module Disconnect : HANDLER);
  "terminate", (module Terminate : HANDLER);
] |> List.to_seq
  |> Hashtbl.of_seq


let handle_exn t config message =
  let aux command =
    let h = Hashtbl.find t command in
    let module H = MakeHandler (val h : HANDLER) in
    let h = H.make in
    try%lwt
      let%lwt output = H.handle h ~config message in
      Some output |> Lwt.return
    with Js_msg.Wrong_encoder _ ->
      None |> Lwt.return
  in
  let%lwt output =
    let cmds = [
        "cancel";
        "initialize";
        "configurationDone";
        "launch";
        "attach";
        "restart";
        "disconnect";
        "terminate";
      ] in
    Lwt_list.filter_map_p aux cmds
  in
  (* First one that doesnt error is what we want *)
  let ret =
    match output with
    | [] -> Printf.sprintf "Cannot handle message: '%s'" message |> Result.error
    | output :: _ -> Result.ok output
  in
  Lwt.return ret


let handle_message t config msg =
  match%lwt handle_exn t config msg with
  | Ok _js -> failwith "TODO"
  | Error _err -> failwith "TODO"


let rec main_handler t (config:Dapper.Dap_handler_t.config) ~content_length _flow ic _oc =
  let open Lwt in
  match content_length with
  | Some count ->
      Logs_lwt.info (fun m -> m "[DAP] got count %d" count) >>= fun _ ->
      (* \r\n throw away *)
      Lwt_io.read ~count:2 ic >>= fun header_break ->
      assert (header_break = "\r\n") |> Lwt.return >>= fun _ ->
      Lwt_io.read ~count ic >>= fun msg ->
      Logs_lwt.info (fun m -> m "[DAP] Got message '%s'" msg) >>= fun _ ->
      handle_message t config msg >>= fun _ ->
      main_handler t config ~content_length:None _flow ic _oc
  | None -> (
      Logs_lwt.info (fun m -> m "[DAP] no content length yet") >>= fun _ ->
      Lwt_io.read_line_opt ic >>= function
      | Some msg ->
          let content_length = Dap_header.content_length msg in
          main_handler t config ~content_length _flow ic _oc
      | None -> Logs_lwt.info (fun m -> m "[DAP] connection closed")
          )
