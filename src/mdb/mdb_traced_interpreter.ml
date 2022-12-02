open Protocol
open Alpha_context
open Environment.Error_monad
module Plugin = Tezos_protocol_plugin_014_PtKathma


module T (Cfg : Mdb_types.INTERPRETER_CFG) = struct

  type input = Cfg.input
  type logger = Script_typed_ir.logger

  type log_element =
    | Log :
        context
        * Script.location
        * ('a * 's)
        * ('a, 's) Script_typed_ir.stack_ty
        -> log_element

  let unparse_stack ctxt (stack, stack_ty) =
    let open Lwt_result_syntax in
    (* We drop the gas limit as this function is only used for debugging/errors. *)
    let ctxt = Gas.set_unlimited ctxt in
    let rec unparse_stack :
      type a s.
      (a, s) Script_typed_ir.stack_ty * (a * s) ->
      (Script.expr * string option * bool) list tzresult Lwt.t = function
      | Bot_t, (EmptyCell, EmptyCell) -> return_nil
      | Item_t (ty, rest_ty), (v, rest) ->
        let* (data, _ctxt) = Script_ir_translator.unparse_data ctxt Cfg.unparsing_mode ty v in
        let+ rest = unparse_stack (rest_ty, rest) in
        let data = Environment.Micheline.strip_locations data in
        (data, None, false) :: rest
    in
    unparse_stack (stack_ty, stack)

  let trace_logger ~in_channel:ic () : Script_typed_ir.logger =

    let log : log_element list ref = ref [] in

    let log_interp _ ctxt loc sty stack =
      Logs.info (fun m -> m "log_interp @ location %d" loc);
      log := Log (ctxt, loc, stack, sty) :: !log
    in
    let log_entry _ _ctxt loc _sty _stack =
      Logs.info (fun m -> m "log_entry @ location %d" loc);
      (* block waiting for a \n on in_channel *)
      let msg = input_line ic in
      Logs.info (fun m -> m "got msg '%s'" msg)
    in
    let log_exit _ ctxt loc sty stack =
      Logs.info (fun m -> m "log_exit @ location %d" loc);
      log := Log (ctxt, loc, stack, sty) :: !log
    in
    let log_control _ =
      Logs.info (fun m -> m "log_control");
    in
    let get_log () =
      (* TODO change so that can call this repeatedly
         but it only returns new records *)
      let open Lwt_result_syntax in
      Environment.List.map_es
        (fun (Log (ctxt, loc, stack, stack_ty)) ->
           trace
             Plugin.Plugin_errors.Cannot_serialize_log
             (let* stack = unparse_stack ctxt (stack, stack_ty) in
              let stack = Environment.List.map (fun (expr, _, _) -> expr) stack in
              return (loc, Gas.level ctxt, stack))
        )
        !log
      >>=? fun res -> return (Some (List.rev res))
    in
    {log_exit; log_entry; log_interp; get_log; log_control}

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
