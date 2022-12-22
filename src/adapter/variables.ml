module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event
module Model = Mdb.Mdb_model

module T (S : Types.STATE_READONLY_T) = struct

  module On_request = Dap.Variables.On_request (S)

  let not_structured = 0

  let variables_handler =
    On_request.make ~handler:(fun ~state req ->
        let recs = S.log_records state in
        let args = Req.Message.arguments @@ Req.extract req in
        let vrefs = D.VariablesArguments.variablesReference args in
        Logs.debug (fun m -> m "vrefs %d, default vrefs %d" vrefs Defaults.Vals._THE_VARIABLES_REFERENCE);
        assert (vrefs = Defaults.Vals._THE_VARIABLES_REFERENCE);
        let resp =
          let command = Dap.Commands.variables in
          let gas_name, _gas_var = Defaults.Vals._THE_GAS_LOCAL in
          let stack_name, _stack_var = Defaults.Vals._THE_MICHELSON_STACK_LOCAL in
          let gas_val, stack_val =
            match List.nth_opt recs 0 with
            | None -> "", []
            | Some wrec -> Model.Weevil_json.(wrec.gas, wrec.stack)
          in
          let variables = [
            [D.Variable_.make ~name:gas_name ~value:gas_val ~variablesReference:not_structured ()]; (* 0 here means not structured ie no children? *)
            [D.Variable_.make ~name:stack_name ~value:"" ~variablesReference:not_structured ()]; (* 0 here means not structured ie no children? *)
            stack_val |> List.mapi (fun i sv ->
                D.Variable_.make
                  ~name:(Printf.sprintf "%d:" i)
                  ~value:(String.trim sv)
                  ~variablesReference:not_structured
                  ()
              )
          ] |> List.concat
          in
          let body = D.VariablesResponse_body.make ~variables () in
          Dap.Response.default_response_req command body
        in
        let ret = Dap.Response.variablesResponse resp in
        Dap_result.ok ret)

  let handlers ~state = [
    variables_handler ~state;
  ]

  let on_success ~state:_ = ()
  let on_error ~state:_ = ()

end
