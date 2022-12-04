open Protocol
open Environment.Error_monad
module Plugin = Tezos_protocol_plugin_014_PtKathma
module Log_records = Mdb_log_records

module T (Cfg : Mdb_types.INTERPRETER_CFG) = struct

  type t = Script_typed_ir.logger

  let log_element_to_json log_element =
    let open Lwt_result_syntax in
    let module List = Environment.List in
    let* (loc, gas, exprs) =
      trace
        Plugin.Plugin_errors.Cannot_serialize_log
        (let* stack = Cfg.unparse_stack log_element in
         let stack = List.map (fun (expr, _, _) -> expr) stack in
         let loc = Cfg.get_loc log_element in
         let gas = Cfg.get_gas log_element in
         return (loc, gas, stack))
    in
    (* TODO better handling of mich expressions/tickets *)
    let wrec = Mdb_model.Weevil_record.make loc gas (exprs |> List.map (fun e -> (e, None, false))) in
    let wrec_js = Mdb_model.Weevil_record.to_weevil_json wrec in
    let js = Data_encoding.Json.(
        construct Mdb_model.Weevil_json.enc wrec_js
        |> to_string
        |> Dapper.Dap.Header.wrap
      ) in
    return js

  let log_element_to_out log_element out_channel =
    (* unparse the log_element and wait for the data *)
    let to_out : string option ref = ref None in
    let () =
      Lwt.async (fun () ->
          let open Lwt_syntax in
          let* js = log_element_to_json log_element in
          match js with
          | Ok js ->
            let () = to_out := Some js in
            Lwt.return_unit
          | Error _ -> Lwt.return_unit
        )
    in
    let rec aux () =
      match !to_out with
      | Some s ->
        Printf.(fprintf out_channel "%s\n" s; flush out_channel)
      | None ->
        Unix.sleepf 0.01;
        aux ()
    in
    aux ()

  let trace_logger ~in_channel ~out_channel () =

    let log_interp _ ctxt loc sty stack =
      Logs.info (fun m -> m "log_interp @ location %d" loc);
      let log_element = Cfg.make_log ctxt loc stack sty in
      log_element_to_out log_element out_channel
    in
    let log_entry _ _ctxt loc _sty _stack =
      Logs.info (fun m -> m "log_entry @ location %d" loc);
      (* block waiting for a \n on in_channel *)
      let msg = input_line in_channel in
      Logs.info (fun m -> m "got msg '%s'" msg)
    in
    let log_exit _ ctxt loc sty stack =
      Logs.info (fun m -> m "log_exit @ location %d" loc);
      let log_element = Cfg.make_log ctxt loc stack sty in
      log_element_to_out log_element out_channel
    in
    let log_control _ =
      Logs.info (fun m -> m "log_control");
    in
    let get_log () =
      (* NOTE we dont need to use this anymore *)
      let open Lwt_result_syntax in
      return @@ Some []
    in
    Script_typed_ir.{log_exit; log_entry; log_interp; get_log; log_control}

  let execute ctxt step_constants ~script ~entrypoint ~parameter ~logger =
    let open Script_interpreter in
    execute
      ~logger
      ~cached_script:None
      ctxt
      Cfg.unparsing_mode
      step_constants
      ~script
      ~entrypoint
      ~parameter
      ~internal:true
end
