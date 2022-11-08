module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S:Types.State_intf) = struct

  type state = S.t
  type t = S.t

  let make ?state () = Option.value state ~default:S.make_empty

  let state t = t

  include Types.Includes2
      (Dap.Attach.On_request)
      (Dap.Attach.On_response)

  let attach_handler t =
    Dap.Attach.On_request.make
      ~handler:(
        fun config _req ->
          let body = D.EmptyObject.make () in
          let command = Dap.Commands.attach in
          let resp = Res.attachResponse @@ Res.default_response_opt command body in
          match S.oc t with
          | Some _ ->
            let () = S.set_launch_mode t `Attach in
            Dap_result.ok resp
          | None -> (
              let ip = Dap.Config.backend_ip config  |> Ipaddr_unix.of_inet_addr in
              let port = Dap.Config.backend_port config in
              let%lwt ic, oc = S.connect ip port in
              let () = S.set_io t ic oc in
              match S.oc t with
              | Some _ ->
                (* NOTE dont need to start the stepper as we are in attach mode *)
                let () = S.set_launch_mode t `Attach in
                Dap_result.ok resp
              | None ->
                let error =
                  Printf.sprintf "failed to connect to backend svc on localhost port %d" port
                in
                Res.default_response_error error
                |> Res.errorResponse
                |> Dap_result.error
            )
      )

  let process_handler _t =
    Dap.Attach.On_response.make
      ~handler:(fun _ _resp ->
          let event = Dap.Events.process in
          let startMethod = D.ProcessEvent_body_startMethod.Attach in
          let body =
            D.ProcessEvent_body.make
              ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
              ~startMethod
              ()
          in
          Ev.default_event_req event body
          |> Ev.processEvent
          |> Dap_result.ok
        )

  let handlers =
    convert_handlers
      ~handler1:attach_handler
      ~handler2:process_handler

end

include T (State)
