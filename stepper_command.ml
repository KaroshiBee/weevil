open Protocol
open Alpha_context
(* open Error_monad_operators *)
module DRq = Dapper.Dap_request
module DRs = Dapper.Dap_response



let test_context () =
  Context.init 3 >>=? fun (b, _cs) ->
  Incremental.begin_construction b >>=? fun v ->
  return (Incremental.alpha_ctxt v)

let test_stepping contract logger =
  test_context () >>=? fun ctx ->
  Wcontract_helpers.run_script
    ~logger
    ctx
    contract
    ~storage:"Unit"
    ~parameter:"Unit"
    ()
  >>= function
  | Ok _ ->
    return_unit
  | Error errs ->
    let ss = Format.asprintf "Unexpected error: %a" Error_monad.pp_print_trace errs in
    Lwt.bind (Lwt_io.printl ss) (fun _ -> return_unit)


module Traced_interpreter = struct
  type log_element =
    | Log :
        context
        * Script.location
        * ('a * 's)
        * ('a, 's) Script_typed_ir.stack_ty
        -> log_element

  let unparse_stack ctxt (stack, stack_ty) =
    (* We drop the gas limit as this function is only used for debugging/errors. *)
    (* let ctxt = Gas.set_unlimited ctxt in *)
    let rec unparse_stack :
      type a s.
      (a, s) Script_typed_ir.stack_ty * (a * s) ->
      (Script.expr * string option) list Environment.Error_monad.tzresult Lwt.t = function
      | (Bot_t, (EmptyCell, EmptyCell)) -> return_nil
      | (Item_t (ty, rest_ty, annot), (v, rest)) ->
        Script_ir_translator.unparse_data
          ctxt
          Readable
          ty
          v
        >>=? fun (data, _ctxt) ->
        unparse_stack (rest_ty, rest) >|=? fun rest ->
        let annot =
          match Script_ir_annot.unparse_var_annot annot with
          | [] -> None
          | [a] -> Some a
          | _ -> assert false
        in
        let data = Micheline.strip_locations data in
        (data, annot) :: rest
    in
    unparse_stack (stack_ty, stack)

  let unparse_log (Log (ctxt, loc, stack, stack_ty)) =
    (* trace Cannot_serialize_log (unparse_stack ctxt (stack, stack_ty)) *)
    (unparse_stack ctxt (stack, stack_ty))
    >>=? fun stack -> return (loc, Gas.level ctxt, stack)

  let trace_logger oc () : Script_typed_ir.logger =
    let log : log_element list ref = ref [] in
    let log_interp _ ctxt loc sty stack =
      Printf.(fprintf oc "\n# log_interp\n"; flush oc);
      log := Log (ctxt, loc, stack, sty) :: !log
    in
    let log_entry _ _ctxt _loc _sty _stack =
      Printf.(fprintf oc "# log_entry\n"; flush oc);
      let msg = read_line () in
      Printf.(fprintf oc "# got '%s'\n" msg; flush oc);
    in
    let log_exit _ ctxt loc sty stack =
      Printf.(fprintf oc "# log_exit\n"; flush oc);
      let l = Log (ctxt, loc, stack, sty) in
      let _ = unparse_log l
        >>=? fun (loc, gas, expr) -> return @@ Model.Weevil_record.make loc gas expr
        >>=? fun wrec ->
        let (loc_str, gas_str, expr_str) =
          Model.Weevil_record.(
            get_location wrec, get_gas wrec, get_expr_str wrec
          ) in
        return @@ Printf.(fprintf oc "{\"location\": %s, \"gas\": %s, \"stack\": [%s]}\n" loc_str gas_str expr_str)
      in
      log := l :: !log
    in
    let log_control _ =
      Printf.(fprintf oc "# log_control\n"; flush oc);
    in
    let get_log () =
      List.map_es unparse_log !log
      >>=? fun res -> return (Some (List.rev res))
    in
    {log_exit; log_entry; log_interp; get_log; log_control}

end


let process log_file_arg =
  let log_file = match log_file_arg with
  | None -> Defaults._DEFAULT_LOG_FILE
  | Some log_file -> log_file
  in
  let oc = open_out log_file in
  (* let contract = *)
  (*   "{parameter unit;storage unit;code {DROP; UNIT; NIL operation; PAIR}}" in *)
  let contract =
    "{parameter unit;storage unit;code {DROP; PUSH mutez 2944023901536524477; \
     PUSH nat 2; MUL; PUSH mutez 100; ADD; DROP; UNIT; NIL operation; PAIR}}" in

  let logger = Traced_interpreter.trace_logger oc () in

  let stepper = test_stepping contract logger >|= (fun _ -> `Ok ()) in

  Lwt_main.run stepper


module Term = struct
  let log_file_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The log file that the weevil stepper will write to.  \
        If not given defaults to '%s'" Defaults._DEFAULT_LOG_FILE

    in
    Arg.(
      value & pos 0 (some string) None & info [] ~doc ~docv:"FILE"
    )

  let term =
    Cmdliner.Term.(
      ret
        (const process $ log_file_arg)
    )

end

module Manpage = struct
  let command_description =
    "Run the Weevil stepper for the debugger (used by the Weevil service)"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "stepper"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
