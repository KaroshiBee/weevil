open Protocol
open Environment
open Alpha_context
open Environment.Error_monad

module type INTERPRETER_CFG = sig
  type input
  type output
  val unparsing_mode : Script_ir_translator.unparsing_mode
  val to_string_input : input -> string
  val from_string_input : string -> input tzresult

  val to_string_output : output -> string
  val from_string_output : string -> output tzresult

end

module type INTERPRETER = sig

  type input
  type logger = Script_typed_ir.logger

  val trace_logger :
    in_channel:in_channel -> unit -> logger

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
