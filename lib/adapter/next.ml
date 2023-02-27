module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.STATE_READONLY_T) = struct

  module On_request = Dap.Next.On_request (S)
  module On_stopped = Dap.Next.Raise_stopped (S)

  let next_handler =
    On_request.make ~handler:(fun ~state _req ->
        match S.backend_oc state with
        | Some oc ->
          let mich_step = Mdb.Mdb_event.(make ~event:(Step {step_size=1}) ()) in
          let mich_msg = Data_encoding.Json.(construct Mdb.Mdb_event.enc mich_step |> to_string |> Dap.Header.wrap) in
          let%lwt () = Lwt_io.write oc mich_msg in
          let resp =
            let command = Dap.Commands.next in
            let body = D.EmptyObject.make () in
            Dap.Response.default_response_opt command body
          in
          let ret = Dap.Response.nextResponse resp in
          Dap_result.ok ret
        | _ -> Dap_result.from_error_string "Cannot connect to backend"
      )

  let stopped_handler =
    On_stopped.make ~handler:(fun ~state:_ _ ->
        let ev =
          let event = Dap.Events.stopped in
          let reason = D.StoppedEvent_body_reason.Step in
          let body =
            D.StoppedEvent_body.make
              ~reason
              ~threadId:Dap.Defaults._THE_THREAD_ID
              ~preserveFocusHint:true
              ~allThreadsStopped:true
              ()
          in
          Ev.default_event_req event body
        in
        let ret = Ev.stoppedEvent ev in
        Dap_result.ok ret)

  let handlers ~state = [
    next_handler ~state;
    stopped_handler ~state;
  ]

  let on_success ~state:_ = ()
  let on_error ~state:_ = ()

end
