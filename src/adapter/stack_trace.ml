module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event
module Model = Mdb.Mdb_model
module Mdb_cfg = Mdb.Mdb_types.Mich_config

module Mdb_ = struct
  (* need a way to break out of handle message *)
  exception Get_records of Model.Weevil_json.t list

  let get_recs ic oc =
    (* TODO is this a list or single? *)
    let enc = Data_encoding.(option @@ list Model.Weevil_json.enc) in

    let handle_message msg _ic _oc =
      let%lwt () = Logs_lwt.debug (fun m -> m "[DAP-stacktrace] got msg from subprocess '%s'" msg) in
      match%lwt Mdb.Mdb_server.read_weevil_recs ~enc msg with
      | Some (Some wrecs) ->
        let%lwt () = Logs_lwt.debug (fun m -> m "[DAP-stacktrace] got %d weevil log records from mdb" @@ List.length wrecs) in
        raise @@ Get_records wrecs
      | _ -> Lwt.return_unit
    in

    let recs = ref [] in

    let%lwt () =
      try%lwt
        Dap.content_length_message_handler
          ~name:"DAP-stacktrace"
          ~handle_message
          ~content_length:None
          ic
          oc
      with
      | Get_records wrecs ->
        let () = recs := wrecs in
        Lwt.return_unit

    in
    Lwt.return !recs
end

module T (S : Types.STATE_T) = struct

  module On_request = Dap.Stack_trace.On_request (S)

  let stack_trace_handler =
    On_request.make ~handler:(fun ~state req ->
        match (S.backend_ic state, S.backend_oc state) with
        | (Some ic, Some oc) ->
          let args = Req.Message.arguments @@ Req.extract req in
          let threadId = D.StackTraceArguments.threadId args in
          assert (threadId = Defaults.Vals._THE_THREAD_ID);

          let mich_get_recs = Mdb.Mdb_event.(make ~event:(GetRecords {get_records=()}) ()) in
          let mich_msg = Data_encoding.Json.(construct Mdb.Mdb_event.enc mich_get_recs |> to_string |> Dap.Header.wrap) in
          let%lwt () = Lwt_io.write oc mich_msg in
          let%lwt recs = Mdb_.get_recs ic oc in
          let () = S.set_new_log_records state recs in

          (* make sure to get the master set of recs from the state - does stuff like sorting and dedup *)
          let recs = S.log_records state in
          let resp =
            let command = Dap.Commands.stackTrace in
            let stackFrames =
              match (S.mdb_config state, List.nth_opt recs 0) with
              | (_, None) | (None, _) -> []
              | (Some Mdb_cfg.{script_filename; entrypoint; _}, Some wrec) ->
                let loc = wrec.location in
                let source = D.Source.make
                    ~name:(Filename.basename script_filename)
                    ~path:script_filename
                    ~presentationHint:D.Source_presentationHint.Normal
                    ()
                in
                [D.StackFrame.make
                   ~id:Defaults.Vals._THE_FRAME_ID
                   ~name:entrypoint
                   ~source
                   ~line:loc.start.line
                   ~column:loc.start.column
                   ~endLine:loc.stop.line
                   ~endColumn:loc.stop.column
                   ~presentationHint:D.StackFrame_presentationHint.Normal
                   ()]
            in
            let totalFrames = List.length stackFrames in
            assert (totalFrames <= 1); (* NOTE can be zero if stepping hasnt started yet *)

            let body = D.StackTraceResponse_body.make ~stackFrames ~totalFrames () in
            Dap.Response.default_response_req command body
          in
          let ret = Dap.Response.stackTraceResponse resp in
          Dap_result.ok ret
        | _, _ -> Dap_result.from_error_string "Cannot connect to backend"
      )

  let handlers ~state = [
    stack_trace_handler ~state;
  ]

  let on_success ~state:_ = ()
  let on_error ~state:_ = ()

end
