module Dap = Dapper.Dap
module D = Dap.Data
module Dap_result = Dapper.Dap_result
module Req = Dap.Request
module Res = Dap.Response
module Ev = Dap.Event
module Model = Mdb.Mdb_model
module Mich_config = Mdb.Mdb_config

module T (S : Types.STATE_READONLY_T) = struct

  module On_request = Dap.Variables.On_request (S)

  let not_structured = 0

  let variables_handler =
    (* I think we are suppposed to retreive the variables with vref=XX *)
    On_request.make ~handler:(fun ~state req ->
        let recs = S.log_records state in
        let args = Req.Message.arguments @@ Req.extract req in
        let vref = D.VariablesArguments.variablesReference args in
        let () = Logs.debug (fun m -> m "requested vref %d" vref) in

        let resp =
          let command = Dap.Commands.variables in
          let params_name, params_var = Defaults.Vals._THE_PARAMETERS_LOCAL in
          let storage_name, storage_var = Defaults.Vals._THE_STORAGE_LOCAL in
          let params_val = S.mdb_config state |> Option.map (fun Mich_config.{parameter; _} -> parameter) |> Option.value ~default:"unknown" in
          let storage_val = S.mdb_config state |> Option.map (fun Mich_config.{storage; _} -> storage) |> Option.value ~default:"unknown" in
          let gas_name, gas_var = Defaults.Vals._THE_GAS_LOCAL in
          let stack_name, stack_var = Defaults.Vals._THE_MICHELSON_STACK_LOCAL in
          let gas_val, stack_val =
            match List.nth_opt recs 0 with
            | None -> "", []
            | Some wrec -> Model.(gas wrec, stack wrec)
          in
          let variables =
            match Defaults.Vals.classify_vref_exn vref with
            | `Args ->
              let () = Logs.debug (fun m -> m "requested Args %d" vref) in
              [
                (* D.Variable_.make ~name:params_name ~value:"" ~variablesReference:params_var (); *)
                (* D.Variable_.make ~name:storage_name ~value:"" ~variablesReference:storage_var (); *)
              ]
            | `Locals ->
              let () = Logs.debug (fun m -> m "requested Locals %d" vref) in
              [
                (* NOTE really important that the value is not "" for nodes with children *)
                D.Variable_.make ~name:params_name ~value:".." ~variablesReference:params_var ();
                D.Variable_.make ~name:storage_name ~value:".." ~variablesReference:storage_var ();
                D.Variable_.make ~name:gas_name ~value:".." ~variablesReference:gas_var ();
                D.Variable_.make ~name:stack_name ~value:"[..]" ~variablesReference:stack_var ();
              ]
            | `Params ->
              let () = Logs.debug (fun m -> m "requested input params %d" vref) in
              [
                D.Variable_.make ~name:"Input" ~value:params_val ~variablesReference:not_structured ();
              ]
            | `Storage ->
              let () = Logs.debug (fun m -> m "requested storage %d" vref) in
              [
                D.Variable_.make ~name:"Storage" ~value:storage_val ~variablesReference:not_structured ();
              ]
            | `Gas ->
              let () = Logs.debug (fun m -> m "requested Gas %d" vref) in
              [
                D.Variable_.make ~name:"Limited" ~value:gas_val ~variablesReference:not_structured ();
              ]
            | `Mich_stack ->
              let () = Logs.debug (fun m -> m "requested Michelson stack %d" vref) in
              stack_val |> List.mapi (fun i sv ->
                  D.Variable_.make
                    ~name:(Printf.sprintf "[%d]" i)
                    ~value:(String.trim sv)
                    ~variablesReference:not_structured
                    ()
                )
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
