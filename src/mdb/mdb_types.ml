module Tez = Mdb_tezos.Tez014

module type INTERPRETER_CFG_T = sig

  type t

  val make_log :
    Tez.Ctxt.context ->
    Tezos_micheline.Micheline_parser.location ->
    ('a * 's) ->
    ('a, 's) Tez.Prot.Script_typed_ir.stack_ty ->
    t

  val unparsing_mode : Tez.Prot.Script_ir_translator.unparsing_mode

  val unparse_stack :
    t ->
    (Tez.Ctxt.Script.expr * string option * bool) list Tez.Err.tzresult Lwt.t

  val get_loc :
    t -> Tezos_micheline.Micheline_parser.location

  val get_gas :
    t -> Tez.Ctxt.Gas.t

end


module type INTERPRETER_T = sig

  type t

  val trace_interp :
    in_channel:in_channel ->
    out_channel:out_channel ->
    Mdb_file_locations.t ->
    t

  val execute :
    Tez.Ctxt.context ->
    Tez.Prot.Script_typed_ir.step_constants ->
    script:Tez.Ctxt.Script.t ->
    entrypoint:Tez.Ctxt.Entrypoint.t ->
    parameter:Tez.Ctxt.Script.expr ->
    interp:t ->
    (Tez.Prot.Script_interpreter.execution_result * Tez.Ctxt.context) Tez.Err.tzresult Lwt.t

end


module type STEPPER_T = sig

  type interp
  type t

  val code_trace : t -> (
      Tez.Ctxt.Script.expr *
      Tez.Prot.Apply_internal_results.packed_internal_contents list *
      Tez.Ctxt.Lazy_storage.diffs option
    ) option

  val chain_id : t -> Tezos_crypto.Chain_id.t
  val alpha_context : t -> Tez.Ctxt.context
  val mock_context : t -> Tezos_client_base_unix.Client_context_unix.unix_mockup

  val init :
    protocol_str:string ->
    base_dir:string ->
    unit ->
    t tzresult Lwt.t

  val typecheck :
    script_filename:string ->
    storage:string ->
    input:string ->
    entrypoint:string ->
    t ->
    (Mdb_typechecker.t * Mdb_typechecker.t * Mdb_typechecker.t * Tez.Ctxt.Entrypoint.t) tzresult Lwt.t

  val step :
    make_interp:(Mdb_file_locations.t -> interp) ->
    script:Mdb_typechecker.t ->
    storage:Mdb_typechecker.t ->
    input:Mdb_typechecker.t ->
    entrypoint:Tez.Ctxt.Entrypoint.t ->
    t ->
    t tzresult Lwt.t

end
