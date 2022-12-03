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


module type INTERPRETER = sig

  type t
  type log_records

  val trace_logger :
    ?full_trace:bool ->
    ?log_records:log_records ->
    in_channel:in_channel ->
    unit ->
    t

  val get_execution_trace_updates :
    t -> Script_typed_ir.execution_trace tzresult Lwt.t

  val execute :
    Alpha_context.t ->
    Script_typed_ir.step_constants ->
    script:Script.t ->
    entrypoint:Entrypoint_repr.t ->
    parameter:Script.expr ->
    logger:t ->
    (Script_interpreter.execution_result * Alpha_context.t) tzresult Lwt.t

end
