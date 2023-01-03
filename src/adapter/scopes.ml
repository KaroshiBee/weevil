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
        assert (Defaults.Vals._THE_FRAME_ID = D.ScopesArguments.frameId args);
        let resp =
          let command = Dap.Commands.scopes in
          let locals_name, locals_var = Defaults.Vals._THE_ONLY_SCOPE in
          (* let gas_name, gas_var = Defaults.Vals._THE_GAS_LOCAL in *)
          (* let stack_name, stack_var = Defaults.Vals._THE_MICHELSON_STACK_LOCAL in *)
          let scopes = [
            D.Scope.make
              ~name:locals_name
              ~presentationHint:D.Scope_presentationHint.Locals
              ~variablesReference:locals_var
              ~namedVariables:2 (* NOTE always the GAS and MICH STACK *)
              ~expensive:false
              ();
            (* D.Scope.make *)
            (*   ~name:gas_name *)
            (*   ~presentationHint:D.Scope_presentationHint.Locals *)
            (*   ~variablesReference:gas_var *)
            (*   ~expensive:false *)
            (*   (); *)
            (* D.Scope.make *)
            (*   ~name:stack_name *)
            (*   ~presentationHint:D.Scope_presentationHint.Locals *)
            (*   ~variablesReference:stack_var *)
            (*   ~expensive:false *)
            (*   () *)
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
