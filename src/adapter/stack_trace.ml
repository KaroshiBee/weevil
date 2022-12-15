module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event
module Model = Mdb.Mdb_model

module Mdb_ = struct
  (* inspired by effect handlers - need a way to break out of handle message *)
  exception Get_records of Model.Weevil_json.t list

  let get_recs ic oc =
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
    On_request.make ~handler:(fun ~state _req ->
        match (S.backend_ic state, S.backend_oc state) with
        | (Some ic, Some oc) ->
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
              match List.nth_opt recs 0 with
              | None -> []
              | Some wrec ->
                let loc = Mdb.Mdb_model.Weevil_json.relative_loc wrec in
                (* TODO put these filenames on the wrec? *)
                let source = D.Source.make ~name:"example.tz" ~path:"/home/wyn/dev/weevil/example.tz" () in
                [D.StackFrame.make
                   ~id:Defaults.Vals._THE_FRAME_ID
                   ~name:Defaults.Vals._THE_FRAME_NAME
                   ~source
                   ~line:loc
                   ~column:0
                   ()]
            in
            let totalFrames = List.length stackFrames in
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

  let on_handled ~state:_ = ()

end
