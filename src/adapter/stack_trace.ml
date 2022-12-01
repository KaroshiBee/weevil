module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.STATE_READONLY_T) = struct

  module On_request = Dap.Stack_trace.On_request (S)

  let stack_trace_handler =
    On_request.make ~handler:(fun ~state:_ _req ->
        let resp =
          let command = Dap.Commands.stackTrace in
          let stackFrames = [] in
          (*   match List.nth_opt !recs 0 with *)
          (*   | None -> [] *)
          (*   | Some wrec -> *)
          (*     let loc = Model.Weevil_json.relative_loc wrec in *)
          (*     let source = Db.Source.make ~name:"example.tz" ~path:"/home/wyn/dev/weevil/example.tz" () in *)
          (*     [D.StackFrame.make *)
          (*        ~id:Defaults.Vals._THE_FRAME_ID *)
          (*        ~name:Defaults.Vals._THE_FRAME_NAME *)
          (*        ~source *)
          (*        ~line:loc *)
          (*        ~column:0 *)
          (*        ()] *)
          (* in *)
          let totalFrames = List.length stackFrames in
          let body = D.StackTraceResponse_body.make ~stackFrames ~totalFrames () in
          Dap.Response.default_response_req command body
        in
        let ret = Dap.Response.stackTraceResponse resp in
        Dap_result.ok ret)

  let handlers ~state = [
    stack_trace_handler ~state;
  ]

end
