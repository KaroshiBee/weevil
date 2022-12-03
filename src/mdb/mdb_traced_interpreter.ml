open Protocol
open Environment.Error_monad
module Plugin = Tezos_protocol_plugin_014_PtKathma
module LocHashtbl = Mdb_types.LocHashtbl

module T (Cfg : Mdb_types.INTERPRETER_CFG) = struct

  type logger = Script_typed_ir.logger
  type log_element = Cfg.t
  type log_record = | New of log_element | Old of log_element
  let make_new l = New l
  let make_old l = Old l

  type log_elements = log_record LocHashtbl.t

  let trace_logger ?(full_trace=false) ?log_elements:log_elements ~in_channel:ic () =

    (* NOTE storage for the logs,
       new ones are Hashtbl.add'd because for now we keep everything,
       maybe later we will remove old ones as we go ? *)
    let log_elements : log_elements = Option.value log_elements ~default:(LocHashtbl.create 500) in

    let log_interp _ ctxt loc sty stack =
      Logs.info (fun m -> m "log_interp @ location %d" loc);
      let () = if LocHashtbl.mem log_elements loc then Logs.debug (fun m -> m "log_interp @ overwriting location %d with new log record" loc) else () in
      LocHashtbl.add log_elements loc @@ make_new @@ Cfg.make_log ctxt loc stack sty
    in
    let log_entry _ _ctxt loc _sty _stack =
      Logs.info (fun m -> m "log_entry @ location %d" loc);
      (* block waiting for a \n on in_channel *)
      let msg = input_line ic in
      Logs.info (fun m -> m "got msg '%s'" msg)
    in
    let log_exit _ ctxt loc sty stack =
      Logs.info (fun m -> m "log_exit @ location %d" loc);
      (* NOTE with looping constructs in mich you will overwrite the same locs again *)
      let () = if LocHashtbl.mem log_elements loc then Logs.debug (fun m -> m "log_exit @ overwriting location %d with new log record" loc) else () in
      LocHashtbl.add log_elements loc @@ make_new @@ Cfg.make_log ctxt loc stack sty
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
        LocHashtbl.to_seq log_elements
        |> List.of_seq
        |> List.sort (fun (locX, _) (locY, _) -> Int.compare locX locY)
        |> List.filter_map_es
          (function
            | (_, Old _ ) -> return None
            | (_, New log_element) ->
              trace
                Plugin.Plugin_errors.Cannot_serialize_log
                (let* stack = Cfg.unparse_stack log_element in
                 let stack = List.map (fun (expr, _, _) -> expr) stack in
                 let loc = Cfg.get_loc log_element in
                 let gas = Cfg.get_gas log_element in
                 return @@ Some (loc, gas, stack))
          )
      in
      (* update all the New ones to Old, if full_trace then keep old ones as old *)
      let () = LocHashtbl.filter_map_inplace (fun _ky -> function
          | New l -> Some (make_old l)
          | Old _ as l -> if full_trace then Some l else None
        ) log_elements
      in
      return @@ Some res
    in
    Script_typed_ir.{log_exit; log_entry; log_interp; get_log; log_control}

  let get_execution_trace_updates (logger:logger) =
    let open Lwt_result_syntax in
    let* trace = logger.get_log () in
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
