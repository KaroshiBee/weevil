module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.STATE_READONLY_T) = struct

  module On_request = Dap.Threads.On_request (S)

  let threads_handler =
    On_request.make ~handler:(fun ~state:_ _req ->
        let resp =
          let command = Dap.Commands.threads in
          let id = Dap.Defaults._THE_THREAD_ID in
          let threads = [D.Thread.make ~id ~name:"main thread" ()] in
          let body = D.ThreadsResponse_body.make ~threads () in
          Dap.Response.default_response_req command body
        in
        let ret = Dap.Response.threadsResponse resp in
        Dap_result.ok ret)

  let handlers ~state = [
    threads_handler ~state;
  ]

  let on_success ~state:_ = ()
  let on_error ~state:_ = ()

end
