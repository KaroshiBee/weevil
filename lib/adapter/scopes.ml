module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event

module T (S : Types.STATE_READONLY_T) = struct

  module On_request = Dap.Scopes.On_request (S)

  let scopes_handler =
    On_request.make ~handler:(fun ~state:_ req ->
        let args = Req.Message.arguments @@ Req.extract req in
        assert (Dap.Defaults._THE_FRAME_ID = D.ScopesArguments.frameId args);
        let resp =
          let command = Dap.Commands.scopes in
          let locals_name, locals_var = Dap.Defaults._THE_LOCALS_SCOPE in
          (* let args_name, args_var = Defaults.Vals._THE_ARGS_SCOPE in *)
          let scopes = [
            D.Scope.make
              ~name:locals_name
              ~presentationHint:D.Scope_presentationHint.Locals
              ~variablesReference:locals_var
              (* ~namedVariables:2 (\* NOTE always the GAS and MICH STACK *\) *)
              ~expensive:false
              ();
            (* D.Scope.make *)
            (*   ~name:args_name *)
            (*   ~presentationHint:D.Scope_presentationHint.Arguments *)
            (*   ~variablesReference:args_var *)
            (*   (\* ~namedVariables:2 (\\* NOTE always params and storage *\\) *\) *)
            (*   ~expensive:false *)
            (*   (); *)
          ] in
          let body = D.ScopesResponse_body.make ~scopes () in
          Dap.Response.default_response_req command body
        in
        let ret = Dap.Response.scopesResponse resp in
        Dap_result.ok ret)

  let handlers ~state = [
    scopes_handler ~state;
  ]

  let on_success ~state:_ = ()
  let on_error ~state:_ = ()

end
