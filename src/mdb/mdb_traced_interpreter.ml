open Protocol
open Alpha_context
open Environment.Error_monad
module Plugin = Tezos_protocol_plugin_014_PtKathma


module T (Unparsing_mode : Mdb_types.UNPARSING_MODE) = struct

  type log_element =
    | Log :
        context
        * Script.location
        * ('a * 's)
        * ('a, 's) Script_typed_ir.stack_ty
        -> log_element

  let unparse_stack ctxt (stack, stack_ty) =
    (* We drop the gas limit as this function is only used for debugging/errors. *)
    let ctxt = Gas.set_unlimited ctxt in
    let rec unparse_stack :
      type a s.
      (a, s) Script_typed_ir.stack_ty * (a * s) ->
      Script.expr list tzresult Lwt.t = function
      | Bot_t, (EmptyCell, EmptyCell) -> return_nil
      | Item_t (ty, rest_ty), (v, rest) ->
        Script_ir_translator.unparse_data
          ctxt
          Unparsing_mode.unparsing_mode
          ty
          v
        >>=? fun (data, _ctxt) ->
        unparse_stack (rest_ty, rest) >|=? fun rest ->
        let data = Environment.Micheline.strip_locations data in
        data :: rest
    in
    unparse_stack (stack_ty, stack)

  let trace_logger ~msg_mvar () : Script_typed_ir.logger =
    let log : log_element list ref = ref [] in
    let log_interp _ ctxt loc sty stack =
      Logs.info (fun m -> m "log_interp @ location %d" loc);
      log := Log (ctxt, loc, stack, sty) :: !log
    in
    let log_entry _ _ctxt loc _sty _stack =
      Logs.info (fun m -> m "log_entry @ location %d" loc);
      let msg = Lwt_preemptive.run_in_main (fun () ->
          let%lwt () = Logs_lwt.info (fun m -> m "trying to get mvar") in
          Lwt_mvar.take msg_mvar
        ) in
      Logs.info (fun m -> m "log_entry got '%s'" msg)
    in
    let log_exit _ ctxt loc sty stack =
      Logs.info (fun m -> m "log_exit @ location %d" loc);
      log := Log (ctxt, loc, stack, sty) :: !log
    in
    let log_control _ =
      Logs.info (fun m -> m "log_control");
    in
    let get_log () =
      Environment.List.map_es
        (fun (Log (ctxt, loc, stack, stack_ty)) ->
           trace
             Plugin.Plugin_errors.Cannot_serialize_log
             (unparse_stack ctxt (stack, stack_ty))
           >>=? fun stack -> return (loc, Gas.level ctxt, stack))
        !log
      >>=? fun res -> return (Some (List.rev res))
    in
    {log_exit; log_entry; log_interp; get_log; log_control}

  let execute ctxt step_constants ~script ~entrypoint ~parameter ~logger =
    let open Script_interpreter in
    execute
      ~logger
      ~cached_script:None
      ctxt
      Unparsing_mode.unparsing_mode
      step_constants
      ~script
      ~entrypoint
      ~parameter
      ~internal:true
    >>=? fun res ->
    logger.get_log () >|=? fun trace ->
    let trace = Option.value ~default:[] trace in
    (res, trace)
end
