open Protocol
open Environment.Error_monad
module Plugin = Tezos_protocol_plugin_014_PtKathma
module Log_records = Mdb_log_records

module T (Cfg : Mdb_types.INTERPRETER_CFG) = struct

  type t = Script_typed_ir.logger
  type log_records = Cfg.t Log_records.t

  let trace_logger ?(full_trace=false) ?log_records ~in_channel () =

    (* NOTE storage for the logs,
       new ones are Hashtbl.add'd because for now we keep everything,
       maybe later we will remove old ones as we go ? *)
    let log_records = Option.value log_records ~default:(Log_records.make ()) in

    let log_interp _ ctxt loc sty stack =
      Logs.info (fun m -> m "log_interp @ location %d" loc);
      let () = if Log_records.mem log_records loc then Logs.debug (fun m -> m "log_interp @ overwriting location %d with new log record" loc) else () in
      Log_records.add_new log_records loc @@ Cfg.make_log ctxt loc stack sty
    in
    let log_entry _ _ctxt loc _sty _stack =
      Logs.info (fun m -> m "log_entry @ location %d" loc);
      (* block waiting for a \n on in_channel *)
      let msg = input_line in_channel in
      Logs.info (fun m -> m "got msg '%s'" msg)
    in
    let log_exit _ ctxt loc sty stack =
      Logs.info (fun m -> m "log_exit @ location %d" loc);
      (* NOTE with looping constructs in mich you will overwrite the same locs again *)
      let () = if Log_records.mem log_records loc then Logs.debug (fun m -> m "log_exit @ overwriting location %d with new log record" loc) else () in
      Log_records.add_new log_records loc @@ Cfg.make_log ctxt loc stack sty
    in
    let log_control _ =
      Logs.info (fun m -> m "log_control");
    in
    let get_log () =
      (* NOTE can call this repeatedly
         but it should only returns new records *)
      let open Lwt_result_syntax in
      let module List = Environment.List in
      let* res =
        Log_records.to_list log_records
        |> List.map_es
          (fun log_element ->
             trace
               Plugin.Plugin_errors.Cannot_serialize_log
               (let* stack = Cfg.unparse_stack log_element in
                let stack = List.map (fun (expr, _, _) -> expr) stack in
                let loc = Cfg.get_loc log_element in
                let gas = Cfg.get_gas log_element in
                return (loc, gas, stack))
          )
      in
      (* update all the New ones to Old, if full_trace then keep old ones as old *)
      let () = Log_records.new_to_old_inplace ~keep_old:full_trace log_records
      in
      return @@ Some res
    in
    Script_typed_ir.{log_exit; log_entry; log_interp; get_log; log_control}

  let get_execution_trace_updates Script_typed_ir.{get_log; _} =
    let open Lwt_result_syntax in
    let* trace = get_log () in
    let trace = Option.value trace ~default:[] in
    return trace

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
