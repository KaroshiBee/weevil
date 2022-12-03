open Protocol
open Environment
open Alpha_context
open Environment.Error_monad

module type INTERPRETER_CFG = sig
  type t

  val make_log :
    context ->
    Script.location ->
    ('a * 's) ->
    ('a, 's) Script_typed_ir.stack_ty ->
    t

  val unparsing_mode : Script_ir_translator.unparsing_mode

  val unparse_stack :
    t ->
    (Script.expr * string option * bool) list tzresult Lwt.t

  val get_loc :
    t -> Script.location

  val get_gas :
    t -> Gas.t
end

module LocHashtbl = Hashtbl.Make(struct
    type t = Script.location
    let equal i j = i=j
    let hash i = i land max_int
  end)

module type INTERPRETER = sig

  type logger
  type log_record

  val trace_logger :
    ?full_trace:bool ->
    ?log_elements:log_record LocHashtbl.t ->
    in_channel:in_channel ->
    unit ->
    logger

  val get_execution_trace_updates :
    logger -> Script_typed_ir.execution_trace tzresult Lwt.t

  val execute :
    Alpha_context.t ->
    Script_typed_ir.step_constants ->
    script:Script.t ->
    entrypoint:Entrypoint_repr.t ->
    parameter:Script.expr ->
    logger:logger ->
    (Script_interpreter.execution_result * Alpha_context.t) tzresult Lwt.t

end
