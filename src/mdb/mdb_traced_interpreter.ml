open Protocol
open Alpha_context
open Environment.Error_monad
module Plugin = Tezos_protocol_plugin_014_PtKathma


module T (Cfg : Mdb_types.INTERPRETER_CFG) = struct

  type input = Cfg.input
  type output = Cfg.output
  type logger = Script_typed_ir.logger

  type log_element =
    | Log :
        context
        * Script.location
        * ('a * 's)
        * ('a, 's) Script_typed_ir.stack_ty
        -> log_element

  let unparse_stack ~oc:_ ctxt (stack, stack_ty) =
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

  let unparse_log ~oc (Log (ctxt, loc, stack, stack_ty)) =
    let open Lwt_result_syntax in
    trace
      Plugin.Plugin_errors.Cannot_serialize_log
      (let* stack = unparse_stack ~oc ctxt (stack, stack_ty) in
       return (loc, Gas.level ctxt, stack))

  let trace_logger ~in_channel:ic ~out_channel:oc () : Script_typed_ir.logger =

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
      let l = Log (ctxt, loc, stack, sty) in
      (* TODO can we serialise l and send that? then unparse on main thread in order*)
      let _ = Lwt.ignore_result @@ (
          let open Lwt_result_syntax in
          let*! is_sent = (
            let* (_loc, gas, expr) = unparse_log ~oc l in
            let wrec = Mdb_model.Weevil_record.make loc gas expr in
            let wrec_js = Mdb_model.Weevil_record.to_weevil_json wrec in
            let js = Data_encoding.Json.(
                construct Mdb_model.Weevil_json.enc wrec_js
                |> to_string
                |> Dapper.Dap.Header.wrap
              ) in
            return @@ Printf.(fprintf oc "%s\n" js; flush oc)
          )
          in
          match is_sent with
          | Ok () ->
            Lwt.return @@ Logs.info (fun m -> m "sent weevil record")
          | Error errs ->
            Lwt.return @@ Logs.err (fun m -> m "%s" @@ Data_encoding.Json.(construct trace_encoding errs |> to_string))
        )
      in
      log := l :: !log
    in
    let log_control _ =
      Logs.info (fun m -> m "log_control");
    in
    let get_log () =
      let open Lwt_result_syntax in
      Environment.List.map_es
        (fun (Log (ctxt, loc, stack, stack_ty)) ->
           trace
             Plugin.Plugin_errors.Cannot_serialize_log
             (let* stack = unparse_stack ~oc ctxt (stack, stack_ty) in
              let stack = Environment.List.map (fun (expr, _, _) -> expr) stack in
              return (loc, Gas.level ctxt, stack))
        )
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
      Cfg.unparsing_mode
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
