module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.STATE_READONLY_T) = struct

  module On_request = Dap.Scopes.On_request (S)

  let scopes_handler =
    (* TODO scopes request has a framesID - use it *)
    On_request.make ~handler:(fun ~state:_ _req ->
        let resp =
          let command = Dap.Commands.scopes in
          let locals_name, locals_var = Defaults.Vals._THE_ONLY_SCOPE in
          let scopes = [
            D.Scope.make ~name:locals_name ~variablesReference:locals_var ~expensive:false ()
          ] in
          let body = D.ScopesResponse_body.make ~scopes () in
          Dap.Response.default_response_req command body
        in
        let ret = Dap.Response.scopesResponse resp in
        Dap_result.ok ret)

  let handlers ~state = [
    scopes_handler ~state;
  ]

end