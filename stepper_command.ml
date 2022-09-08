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
  let ctx = Gas.set_limit ctx (Gas_limit_repr.Arith.integral_of_int_exn 100) in
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

  let unparse_stack ~oc ctxt (stack, stack_ty) =
    (* We drop the gas limit as this function is only used for debugging/errors. *)
    (* let ctxt = Gas.set_unlimited ctxt in *)
    let rec unparse_stack :
      type a s.
      (a, s) Script_typed_ir.stack_ty * (a * s) ->
      (Script.expr * string option) list Environment.Error_monad.tzresult Lwt.t = function
      | (Bot_t, (EmptyCell, EmptyCell)) -> return_nil
      | (Item_t (ty, rest_ty, annot), (v, rest)) ->
        let _ = match ty with
        | Ticket_t (_cty, _thing) ->
          Printf.(fprintf oc "\n# GOT TICKET TY\n"; flush oc)
        | _ -> ()
        in
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

  let unparse_log ~oc (Log (ctxt, loc, stack, stack_ty)) =
    (* trace Cannot_serialize_log (unparse_stack ctxt (stack, stack_ty)) *)
    (unparse_stack ~oc ctxt (stack, stack_ty))
    >>=? fun stack -> return (loc, Gas.level ctxt, stack)

  let trace_logger oc () : Script_typed_ir.logger =
    let log : log_element list ref = ref [] in
    let log_interp _ ctxt loc sty stack =
      Printf.(fprintf oc "\n# log_interp @ location %d\n" loc; flush oc);
      log := Log (ctxt, loc, stack, sty) :: !log
    in
    let log_entry _ _ctxt loc _sty _stack =
      Printf.(fprintf oc "\n# log_entry @ location %d\n" loc; flush oc);
      let msg = read_line () in
      Printf.(fprintf oc "# got '%s'\n" msg; flush oc);
    in
    (* TODO location here needs to be understood,
       line number is taken to be length !log for now *)
    let log_exit _ ctxt loc_ sty stack =
      let loc = List.length !log in
      Printf.(fprintf oc "# log_exit @ location %d, line %d\n" loc_ loc; flush oc);
      let l = Log (ctxt, loc_, stack, sty) in
      let _ = unparse_log ~oc l
        >>=? fun (_loc, gas, expr) ->
        return @@ Model.Weevil_record.make loc gas expr
        >>=? fun wrec ->
        let wrec = Model.Weevil_record.to_weevil_json wrec in
        let js = Data_encoding.Json.(
            construct Model.Weevil_json.enc wrec
            |> to_string
            |> Defaults._replace "\n" ""
          ) in
        return @@ Printf.fprintf oc "%s\n" js
      in
      log := l :: !log
    in
    let log_control _ =
      Printf.(fprintf oc "# log_control\n"; flush oc);
    in
    let get_log () =
      List.map_es (unparse_log ~oc) !log
      >>=? fun res -> return (Some (List.rev res))
    in
    {log_exit; log_entry; log_interp; get_log; log_control}

end

let rec lines_from_in_channel =
  let read_line_ i = try Some (input_line i) with End_of_file -> None
  in
  fun i acc ->
    match (read_line_ i) with
    | None -> List.rev acc
    | Some s -> lines_from_in_channel i (s :: acc)

let read_file filename () =
  let i = open_in filename in
  try
    let lns = lines_from_in_channel i [] in
    close_in i;
    lns
  with _ ->
    close_in_noerr i;
    []

let process contract_file_arg =
  let contract_text =
    let lns =
      match contract_file_arg with
      | None -> let s = read_line () in [s]
      | Some contract_file -> read_file contract_file ()
    in
    String.concat " " lns
  in

  let logger = Traced_interpreter.trace_logger stdout () in

  let stepper =
    test_stepping contract_text logger >|= (fun _ -> `Ok ()) in

  Lwt_main.run stepper


module Term = struct
  let contract_file_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The contract filename that the weevil stepper will execute.  \
        If not given then expects the contract code text to be passed from \
         the DAP service after initialisation"

    in
    Arg.(
      value & pos 0 (some string) None & info [] ~doc ~docv:"FILE"
    )

  let term =
    Cmdliner.Term.(
      ret
        (const process $ contract_file_arg)
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
