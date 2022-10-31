open Conduit_lwt_unix
open Dapper.Dap_handler_t
open Dapper.Dap_message
module Dap_commands = Dapper.Dap_commands
module Dap_events = Dapper.Dap_events
module Dap_flow = Dapper.Dap_flow
module Dap_config = Dapper.Dap_config


module Request = struct
  type ('command, 'args, 'presence) t = ('command, 'args, 'presence) RequestMessage.t
  type command = Dap_commands.attach
  let command = Dap_commands.attach
  type args = AttachRequestArguments.t
  type presence = RequestMessage.req
  let enc = RequestMessage.enc command AttachRequestArguments.enc
  let ctor = fun req -> AttachRequest req
  let extract = Dap_flow.to_request
end

module Response = struct
  type ('command, 'body, 'presence) t = ('command, 'body, 'presence) ResponseMessage.t
  type command = Dap_commands.attach
  let command = Dap_commands.attach
  type body = EmptyObject.t option
  type presence = ResponseMessage.opt
  let enc = ResponseMessage.enc_opt command EmptyObject.enc
  let ctor = fun resp -> AttachResponse resp
  let extract = Dap_flow.to_response
end

module Event = struct
  type ('event, 'body, 'presence) t = ('event, 'body, 'presence) EventMessage.t
  type event = Dap_events.process
  let event = Dap_events.process
  type body = ProcessEvent_body.t
  type presence = EventMessage.req
  let enc = EventMessage.enc event ProcessEvent_body.enc
  let ctor = fun ev -> ProcessEvent ev
  let extract = Dap_flow.to_event
end

include MakeReqRespIncludes_withEvent (Request) (Response) (Event)

let on_attach_request request =
  match request with
  | AttachRequest req ->
    let resp =
      let command = RequestMessage.command req in
      let body = EmptyObject.make () in
      default_response_opt command body
    in
    let ret = AttachResponse resp in
    Dap_flow.from_response ret
  | _ -> assert false

let on_attach_response response =
  match response with
  | AttachResponse _ ->
    let ev =
      let event = Dap_events.process in
      let startMethod = ProcessEvent_body_startMethod.Attach in
      let body =
        ProcessEvent_body.make
          ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
          ~startMethod
          ()
      in
      default_event_req event body
    in
    let ret = ProcessEvent ev in
    Dap_flow.from_event ret
  | _ -> assert false


let on_bad_request e _request =
  let resp = default_response_error e in
  let ret = ErrorResponse resp in
  Dap_flow.from_response ret

let handle t config req =
  let open Dap_flow in
  let response = request_response req on_attach_request in
  match to_result response, State.oc t with
  | Result.Ok _, Some backend_oc ->
    (* send an echo command to backend *)
    let%lwt () = Lwt_io.write backend_oc Dap_config.(config.backend_echo) in
    let event = Option.some @@ response_event response on_attach_response in
    let () = State.set_launch_mode t `Attach in
    {response; event; error=None} |> Lwt.return
  | Result.Ok _, None -> (
      let port = Dap_config.backend_port config in
      let ip = Dap_config.backend_ip config in
      let client = `TCP (`IP ip, `Port port) in
      let%lwt ctx = init () in
      let%lwt (_, ic, oc) = connect ~ctx client in
      let _ = State.set_io t ic oc in
      match State.oc t with
      | Some _ ->
        (* NOTE dont need to start the stepper as we are in attach mode *)
        let event = Option.some @@ response_event response on_attach_response in
        let () = State.set_launch_mode t `Attach in
        {response; event; error=None} |> Lwt.return
      | None ->
        let error = Option.some @@ raise_error req (on_bad_request @@ Printf.sprintf "failed to connect to backend svc on localhost port %d" port) in
        {response; event=None; error} |> Lwt.return
    )
  | Result.Error err, _ ->
    let error = Option.some @@ raise_error req (on_bad_request err) in
    {response; event=None; error} |> Lwt.return
