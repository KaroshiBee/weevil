(* Tezos_protocol_014_PtKathma opened globally gives the Protocol import *)
module P = Protocol
module Ctx = P.Alpha_context
(* Tezos_014_PtKathma_test_helpers gives Error_monad_operators, Context, Incremental, Expr *)
module M = Tezos_micheline
module Tz = Tezos_base.TzPervasives
open Tz.Error_monad.Legacy_monad_globals (* other bind infix operators *)


let fake_KT1 =
  P.Contract_hash.of_b58check_exn "KT1FAKEFAKEFAKEFAKEFAKEFAKEFAKGGSE2x"

let default_self = fake_KT1

let default_source = Ctx.Contract.Implicit Tz.Signature.Public_key_hash.zero

let default_step_constants =
  P.Script_interpreter.
    {
      source = default_source;
      payer = default_source;
      self = default_self;
      amount = Ctx.Tez.zero;
      balance = Ctx.Tez.zero;
      chain_id = Tz.Chain_id.zero;
      now = P.Script_timestamp.of_zint Z.zero;
      level = P.Script_int.zero_n;
    }

let (>>=??) = Error_monad_operators.(>>=??)

(* need to retain the trace of code locs of any michelson errors *)
module StepperExpr = struct
  include Expr

  exception Expression_from_string_with_locs of Tz.error list

  module Michelson_v1_parser = Tezos_client_014_PtKathma.Michelson_v1_parser

  (** Parse a Michelson expression from string, raising an exception on error,
      in this version we keep hold of the inner errors. *)
  let from_string ?(check_micheline_indentation = false) str : Ctx.Script.expr =
    let ast, errs =
      Michelson_v1_parser.parse_expression ~check:check_micheline_indentation str
    in
    (match errs with
     | [] -> ()
     | lst -> raise @@ Expression_from_string_with_locs lst
    );
    ast.expanded

end

(** Helper function that parses and types a script, its initial storage and
   parameters from strings. It then executes the typed script with the storage
   and parameter and returns the result. *)
let run_script ?logger ctx ?(step_constants = default_step_constants) contract
    ?(entrypoint = Ctx.Entrypoint.default) ~storage ~parameter () =
  let contract_expr = StepperExpr.from_string contract in
  let storage_expr = StepperExpr.from_string storage in
  let parameter_expr = StepperExpr.from_string parameter in
  let script =
    Ctx.Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  P.Script_interpreter.execute
    ?logger
    ctx
    Readable
    step_constants
    ~script
    ~cached_script:None
    ~entrypoint
    ~parameter:parameter_expr
    ~internal:false
  >>=?? fun res -> Tz.Result.return res |> Lwt.return



let test_context () =
  Context.init1 () >>=? fun (b, _cs) ->
  Incremental.begin_construction b >>=? fun v ->
  return (Incremental.alpha_ctxt v)

let test_stepping contract logger =
  test_context () >>=? fun ctx ->
  let ctx = Ctx.Gas.set_limit ctx (Ctx.Gas.Arith.integral_of_int_exn 100) in
  run_script
    ~logger
    ctx
    contract
    ~storage:"Unit"
    ~parameter:"Unit"
    ()


module Traced_interpreter = struct
  type log_element =
    | Log :
        Ctx.context
        * Ctx.Script.location
        * ('a * 's)
        * ('a, 's) P.Script_typed_ir.stack_ty
        -> log_element

  let unparse_stack ~oc ctxt (stack, stack_ty) =

    let rec _unparse_stack :
      type a s.
      (a, s) P.Script_typed_ir.stack_ty * (a * s) ->
      (Ctx.Script.expr * string option * bool) list Environment.Error_monad.tzresult Lwt.t = function
      | (Bot_t, (EmptyCell, EmptyCell)) -> return_nil
      | (Item_t  (ty, rest_ty), (v, rest)) ->
        let is_ticket = match ty with
        | Ticket_t (Pair_t (_l, _r, _, _), _meta) ->
          Printf.(fprintf oc "\n# GOT TICKET TY\n"; flush oc);
          true
        | Option_t (Pair_t (
            (Ticket_t (Pair_t (_l, _r, _, _), _meta)),
            (Ticket_t (Pair_t (_l', _r', _, _), _meta')),
            _, _), _, _) ->
          Printf.(fprintf oc "\n# GOT SPLIT TICKET TY\n"; flush oc);
          true
        | _ -> false
        in
        P.Script_ir_translator.unparse_data
          ctxt
          Readable
          ty
          v
        >>=? fun (data, _ctxt) ->
        _unparse_stack (rest_ty, rest) >|=? fun rest ->
        let annot = None in
        (*   match Script_ir_annot.unparse_var_annot annot with *)
        (*   | [] -> None *)
        (*   | [a] -> Some a *)
        (*   | _ -> assert false *)
        (* in *)
        let data = M.Micheline.strip_locations data in
        (data, annot, is_ticket) :: rest
    in
    _unparse_stack (stack_ty, stack)

  let unparse_log ~oc (Log (ctxt, loc, stack, stack_ty)) =
    (* trace Cannot_serialize_log (unparse_stack ctxt (stack, stack_ty)) *)
    (unparse_stack ~oc ctxt (stack, stack_ty))
    >>=? fun stack -> return (loc, Ctx.Gas.level ctxt, stack)

  let trace_logger ~input_mvar oc () : P.Script_typed_ir.logger =
    let log : log_element list ref = ref [] in
    let log_interp _ ctxt loc sty stack =
      Printf.(fprintf oc "\n# log_interp @ location %d\n" loc; flush oc);
      log := Log (ctxt, loc, stack, sty) :: !log
    in
    let log_entry _ _ctxt loc _sty _stack =
      Printf.(fprintf oc "\n# log_entry @ location %d\n" loc; flush oc);
      let msg = Lwt_preemptive.run_in_main (fun () -> Printf.(fprintf oc "\n# trying to get mvar\n"; flush oc); Lwt_mvar.take input_mvar) in
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
        return @@ Mdb_model.Weevil_record.make loc gas expr
        >>=? fun wrec ->
        let wrec = Mdb_model.Weevil_record.to_weevil_json wrec in
        let js = Data_encoding.Json.(
            construct Mdb_model.Weevil_json.enc wrec
            |> to_string
            |> Dapper.Dap.Header.wrap
          ) in
        return @@ Printf.fprintf oc "%s\n" js
      in
      log := l :: !log
    in
    let log_control _ =
      Printf.(fprintf oc "# log_control\n"; flush oc);
    in
    let get_log () =
      Tz.List.map_es (unparse_log ~oc) !log
      >>=? fun res ->
      let res = res
      |> List.map (fun (loc, gas, exprs) ->
          let exprs = exprs
                      |> List.map (fun (e, _, _) -> e) in (loc, gas, exprs))
      in
      return (Some (List.rev res))
    in
    {log_exit; log_entry; log_interp; get_log; log_control}

end

