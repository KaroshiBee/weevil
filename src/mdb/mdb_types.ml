open Protocol
open Environment
open Alpha_context
open Environment.Error_monad

module type INTERPRETER_CFG = sig
  type input
  type output
  val unparsing_mode : Script_ir_translator.unparsing_mode
  val to_string_input : input -> string
  val to_string_output : output -> string

end

module type INTERPRETER = sig

  (* type log_element = *)
  (*   | Log : *)
  (*       t * int * ('a * 's) * ('a, 's) Script_typed_ir.stack_ty *)
  (*       -> log_element *)

  (* val unparse_stack : *)
  (*   t -> *)
  (*   ('a * 'b) * ('a, 'b) Script_typed_ir.stack_ty -> *)
  (*   Script.expr list tzresult Lwt.t *)

  type input
  type output
  type logger = Script_typed_ir.logger

  val trace_logger :
    input_mvar:input Lwt_mvar.t -> output_mvar:output Lwt_mvar.t -> unit -> logger

  val execute :
    Alpha_context.t ->
    Script_typed_ir.step_constants ->
    script:Script.t ->
    entrypoint:Entrypoint_repr.t ->
    parameter:Script.expr ->
    logger:Script_typed_ir.logger ->
    ( (Script_interpreter.execution_result * Alpha_context.t) * Script_typed_ir.execution_trace,
      error trace ) result Lwt.t
end
