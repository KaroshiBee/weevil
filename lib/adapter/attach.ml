module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.STATE_T) = struct

  module On_request = Dap.Attach.On_request (S)
  module On_response = Dap.Attach.Raise_process (S)
  module On_attached = Dap.Attach.Raise_stopped (S)
  module Utils = State_utils.T (S)


  let attach_handler =
    On_request.make ~handler:(fun ~state req ->
        let args = Req.(Message.arguments @@ extract req) in
        let script_filename = D.AttachRequestArguments.script_filename args in
        let storage = D.AttachRequestArguments.storage args in
        let parameter = D.AttachRequestArguments.parameter args in
        let entrypoint = D.AttachRequestArguments.entrypoint args in
        let mdb_config = Mdb.Mdb_config.make ~script_filename ~storage ~parameter ~entrypoint () in
        let dap_config = S.config state in

        let body = D.EmptyObject.make () in
        let command = Dap.Commands.attach in
        let resp =
          Res.attachResponse @@ Res.default_response_opt command body
        in
        Utils.attach state dap_config mdb_config
        |> Dap_result.map ~f:(fun _ -> resp)
      )

  let process_handler =
    On_response.make ~handler:(fun ~state:_ _resp ->
        let event = Dap.Events.process in
        let startMethod = D.ProcessEvent_body_startMethod.Attach in
        let body =
          D.ProcessEvent_body.make
              (* * The logical name of the process. This is usually the full path to *)
              (* * process's executable file. Example: /home/example/myproj/program.js. *)
            ~name:"TODO PROCESS EVENT NAME e.g. test.tz"
            ~startMethod
            ()
        in
        Ev.default_event_req event body |> Ev.processEvent |> Dap_result.ok)

  let attached_handler =
    On_attached.make ~handler:(fun ~state:_ _ ->
        let ev =
          let event = Dap.Events.stopped in
          let reason = D.StoppedEvent_body_reason.Entry in
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
    attach_handler ~state;
    process_handler ~state;
    attached_handler ~state;
  ]

  let on_success ~state:_ = ()
  let on_error ~state:_ = ()


end
