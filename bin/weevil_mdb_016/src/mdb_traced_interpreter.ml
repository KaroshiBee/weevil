module Log_records = Mdb_log_records
open Mdb_import.Tez

type t = Prot.Script_typed_ir.logger

type cfg_t =
  | Log :
      Ctxt.context
      * Tezos_micheline.Micheline_parser.location
      * ('a * 's)
      * ('a, 's) Prot.Script_typed_ir.stack_ty
      -> cfg_t

let make_log ctxt loc stack stack_ty =
  Log (ctxt, loc, stack, stack_ty)

let unparsing_mode = Prot.Script_ir_unparser.Readable

let unparse_stack = function
  | Log (ctxt_in, _loc, stack, stack_ty) ->
    let open Lwt_result_syntax in
    (* We drop the gas limit as this function is only used for debugging/errors. *)
    let ctxt = Ctxt.Gas.set_unlimited ctxt_in in
    let rec _unparse_stack :
      type a s.
      (a, s) Prot.Script_typed_ir.stack_ty * (a * s) ->
      (Ctxt.Script.expr * string option * bool) list Err.tzresult Lwt.t = function
      | Bot_t, (EmptyCell, EmptyCell) -> return_nil
      | Item_t (ty, rest_ty), (v, rest) ->
        let* (data, _ctxt) =
          Prot.Script_ir_translator.unparse_data ctxt unparsing_mode ty v
        in
        let+ rest = _unparse_stack (rest_ty, rest) in
        (data, None, false) :: rest
    in
    _unparse_stack (stack_ty, stack)

let get_loc = function
  | Log (_, loc, _, _) -> loc

let get_gas = function
  | Log (ctxt, _, _, _) -> Ctxt.Gas.level ctxt

let log_element_to_json log_element =
  let open Lwt_result_syntax in
  let module List = Env.List in
  let* (loc, gas, exprs) =
    Err.trace
      Plugin.Plugin_errors.Cannot_serialize_log
      (let* stack = unparse_stack log_element in
       let stack = List.map (fun (expr, _, _) -> expr) stack in
       let loc = get_loc log_element in
       let gas = get_gas log_element in
       return (loc, gas, stack))
  in
  return (loc, gas, exprs)

let log_element_to_out log_element out_channel =
  (* unparse the log_element and wait for the data *)
  let to_out : (Tezos_micheline.Micheline_parser.location * Ctxt.Gas.t * 'exprs) option ref = ref None in
  let lck = Lwt_mutex.create () in
  let () =
    Lwt.async (fun () ->
        let open Lwt_syntax in
        let* () = Lwt_mutex.with_lock lck (fun () ->
            let* () = Logs_lwt.debug (fun m -> m "async log element to json") in
            let* res = log_element_to_json log_element in
            match res with
            | Ok t ->
              let* () = Logs_lwt.debug (fun m -> m "async log element to json, got data, writing to to_out") in
              let () = to_out := Some t in
              Lwt.return_unit
            | Error err ->
              let s = Format.asprintf "%a" Err.pp_trace err in
              let* () = Logs_lwt.err (fun m -> m "async log element to json, got error %s" s) in
              Lwt.return_unit
          )
        in
        Lwt.return_unit
      )
  in

  let pp_exprs =
    let pp expr =
      (* TODO better handling of mich expressions/tickets *)
      Format.asprintf "%a" PP.print_expr expr
    in
    List.map pp
  in

  let pp_gas gas =
    Format.asprintf "%a" Ctxt.Gas.pp gas
  in

  let rec aux () =
    match Lwt_mutex.is_locked lck, !to_out with
    | false, Some (location, gas, exprs) ->
      let () = Logs.debug (fun m -> m "pp gas") in
      let gas = pp_gas gas in
      let () = Logs.debug (fun m -> m "pp stack") in
      let stack = pp_exprs exprs in
      let () = Logs.debug (fun m -> m "making weevil js record") in
      let wrec_js = Mdb_model.make ~location ~gas ~stack () in
      let js = Data_encoding.Json.(
          construct Mdb_model.enc wrec_js
          |> to_string
          |> Dapper.Dap.Header.wrap
        ) in
      let () = Logs.debug (fun m -> m "writing back child process -> mdb backend") in
      let () = Printf.(fprintf out_channel "%s" js; flush out_channel) in
      let () = Printf.(fprintf out_channel "\n"; flush out_channel) in
      to_out := None

    | true, _ | _, None ->
      let () = Logs.debug (fun m -> m "waiting for to_out") in
      Unix.sleepf 0.1;
      aux ()

  in
  aux ()

    (* let trace_logger ctxt : Script_typed_ir.logger = *)
    (*   Script_interpreter_logging.make *)
    (*     (module struct *)
    (*       let log : log_element list ref = ref [] *)

    (*       let log_interp _ ctxt loc sty stack = *)
    (*         log := Log (ctxt, loc, stack, sty) :: !log *)

    (*       let log_entry _ _ctxt _loc _sty _stack = () *)

    (*       let log_exit _ ctxt loc sty stack = *)
    (*         log := Log (ctxt, loc, stack, sty) :: !log *)

    (*       let log_control _ = () *)

    (*       let get_log () = *)
    (*         List.fold_left_es *)
    (*           (fun (old_ctxt, l) (Log (ctxt, loc, stack, stack_ty)) -> *)
    (*             let consumed_gas = Gas.consumed ~since:old_ctxt ~until:ctxt in *)
    (*             trace *)
    (*               Plugin_errors.Cannot_serialize_log *)
    (*               (unparse_stack ctxt (stack, stack_ty)) *)
    (*             >>=? fun stack -> return (ctxt, (loc, consumed_gas, stack) :: l)) *)
    (*           (ctxt, []) *)
    (*           (List.rev !log) *)
    (*         >>=? fun (_ctxt, res) -> return (Some (List.rev res)) *)
    (*     end) *)


let make ~in_channel ~out_channel file_locations =

  Plugin.Script_interpreter_logging.make
    (module struct
      let log_interp _ ctxt loc sty stack =
        Logs.debug (fun m -> m "log_interp @ location %d" loc);
        let file_loc = Mdb_michelson.File_locations.get file_locations loc in

        let () = file_loc
                 |> Option.map (fun floc ->
                     let log_element = make_log ctxt floc stack sty in
                     log_element_to_out log_element out_channel
                   )
                 |> Option.value ~default:()
        in

        (* block waiting for a \n on in_channel *)
        let msg = input_line in_channel in
        Logs.debug (fun m -> m "got msg '%s'" msg)

      let log_entry _ _ctxt loc _sty _stack =
        Logs.debug (fun m -> m "log_entry @ location %d" loc)

      let log_exit _ ctxt loc sty stack =
        Logs.debug (fun m -> m "log_exit @ location %d" loc);
        let file_loc = Mdb_michelson.File_locations.get file_locations loc in

        let () = file_loc
                 |> Option.map (fun floc ->
                     let log_element = make_log ctxt floc stack sty in
                     log_element_to_out log_element out_channel
                   )
                 |> Option.value ~default:()
        in

        (* block waiting for a \n on in_channel *)
        let msg = input_line in_channel in
        Logs.debug (fun m -> m "got msg '%s'" msg)

      let log_control _ =
        (* i think this is for lambdas etc *)
        Logs.debug (fun m -> m "log_control")

      let get_log () =
        (* NOTE we dont need to use get_log anymore *)
        let open Lwt_result_syntax in
        return @@ Some []
    end)

let execute ctxt step_constants ~script ~entrypoint ~parameter ~interp =
  Prot.Script_interpreter.execute
    ~logger:interp
    ~cached_script:None
    ctxt
    unparsing_mode
    step_constants
    ~script
    ~entrypoint
    ~parameter
    ~internal:true
