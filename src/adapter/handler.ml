open Dapper.Dap_handler_t
module Dap_commands = Dapper.Dap_commands
module Js_msg = Dapper.Dap_js_msg
module Dap_header = Dapper.Dap_header

module type MAKE_HANDLER = sig
  type input
  type output
  type backend

  type t
  val make : (Lwt_process.process_full -> unit Lwt.t) -> t
  val handle : t -> Dapper.Dap_config.t -> string -> string Lwt.t
end

module MakeHandler (H:HANDLER) :
  (MAKE_HANDLER with
    type input := H.input and
    type output := H.output and
    type backend := H.t)

    = struct

  type t = {
    backend: H.t;
    string_to_input : string -> H.input;
    output_to_string : H.output -> (string, string) Result.t Lwt.t ;
    handle : H.t -> Dapper.Dap_config.t -> H.input -> H.output Lwt.t;
  }

  let make callback = {
    backend=H.make_empty callback;
    string_to_input = H.string_to_input;
    output_to_string = H.output_to_string;
    handle = H.handle;
  }

  let handle t config s =
    let%lwt output =
      t.string_to_input s
      |> t.handle t.backend config
    in
    match%lwt t.output_to_string output with
    | Result.Ok msg ->
      Lwt.return msg
    | Result.Error err ->
      Lwt.return err


end

type t = {
  handlers: (string, (module HANDLER)) Hashtbl.t;
}

let make = {
  handlers = [
    "cancel", (module Cancel : HANDLER);
    "initialize", (module Initialize : HANDLER);
    "configurationDone", (module Configuration : HANDLER);
    "launch", (module Launch : HANDLER);
    "attach", (module Attach : HANDLER);
    "restart", (module Restart : HANDLER);
    "disconnect", (module Disconnect : HANDLER);
    "terminate", (module Terminate : HANDLER);
  ] |> List.to_seq
    |> Hashtbl.of_seq;
}

let handle_exn t callback config message =
  let aux command =
    let h = Hashtbl.find t.handlers command in
    let module H = MakeHandler (val h : HANDLER) in
    let h = H.make callback in
    try%lwt
      let%lwt output = H.handle h config message in
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
    | [] -> Printf.sprintf "[DAP] Cannot handle message: '%s'" message |> Result.error
    | output :: _ -> Result.ok output
  in
  Lwt.return ret


