module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.STATE_READONLY_T) = struct

  module Catch_all = Dap.Error.Raise_error (S)

  let catch_all_handler =
    Catch_all.make ~handler:(fun ~state:_ message ->
        let resp =
          Dap.Response.default_response_error message
        in
        let ret = Dap.Response.errorResponse resp in
        (* NOTE this is a catch all so make it Result.Ok *)
        Dap_result.ok ret
      )

  let handlers ~state = [
    catch_all_handler ~state;
  ]

  let on_success ~state:_ = ()
  let on_error ~state:_ = ()

end
