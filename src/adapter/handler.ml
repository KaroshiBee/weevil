open Dapper.Dap_handler_t
module Dap_commands = Dapper.Dap_commands
module Js_msg = Dapper.Dap_js_msg
module Dap_header = Dapper.Dap_header

module type MAKE_HANDLER = sig
  type input
  type output
  type ('ic, 'oc) backend

  type ('ic, 'oc) t
  val make : 'ic -> 'oc -> ('ic, 'oc) t
  val handle : ('ic, 'oc) t -> config:config -> string -> string Lwt.t
end

module MakeHandler (H:HANDLER) :
  (MAKE_HANDLER with
    type input := H.input and
    type output := H.output and
    type ('ic, 'oc) backend := ('ic, 'oc) H.t)

    = struct

  type ('ic, 'oc) t = {
    backend: ('ic, 'oc) H.t;
    string_to_input : string -> H.input;
    output_to_string : H.output -> (string, string) Result.t Lwt.t ;
    handle : ('ic, 'oc) H.t -> config:config -> H.input -> H.output Lwt.t;
  }

  let make ic oc = {
    backend=H.from_channels ic oc;
    string_to_input = H.string_to_input;
    output_to_string = H.output_to_string;
    handle = H.handle;
  }

  let handle t ~config s =
    let%lwt output =
      t.string_to_input s
      |> t.handle t.backend ~config
    in
    match%lwt t.output_to_string output with
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


let handle_exn t ic oc config message =
  let aux command =
    let h = Hashtbl.find t command in
    let module Handler = (val h : HANDLER) in
    let module H = MakeHandler (Handler) in
    let h = H.make ic oc in
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


let handle_message t ic oc config msg =
  match%lwt handle_exn t ic oc config msg with
  | Ok _js -> failwith "TODO"
  | Error _err -> failwith "TODO"


let rec main_handler t (config:Dapper.Dap_handler_t.config) ~content_length _flow ic _oc =
  let backend_ic = Lwt_io.zero in
  let backend_oc = Lwt_io.null in
  let open Lwt in
  match content_length with
  | Some count ->
      Logs_lwt.info (fun m -> m "[DAP] got count %d" count) >>= fun _ ->
      (* \r\n throw away *)
      Lwt_io.read ~count:2 ic >>= fun header_break ->
      assert (header_break = "\r\n") |> Lwt.return >>= fun _ ->
      Lwt_io.read ~count ic >>= fun msg ->
      Logs_lwt.info (fun m -> m "[DAP] Got message '%s'" msg) >>= fun _ ->
      handle_message t backend_ic backend_oc config msg >>= fun _ ->
      main_handler t config ~content_length:None _flow ic _oc
  | None -> (
      Logs_lwt.info (fun m -> m "[DAP] no content length yet") >>= fun _ ->
      Lwt_io.read_line_opt ic >>= function
      | Some msg ->
          let content_length = Dap_header.content_length msg in
          main_handler t config ~content_length _flow ic _oc
      | None -> Logs_lwt.info (fun m -> m "[DAP] connection closed")
          )
