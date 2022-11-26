open Protocol
open Environment
open Alpha_context
open Environment.Error_monad

module type UNPARSING_MODE = sig
  val unparsing_mode : Script_ir_translator.unparsing_mode
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

  val trace_logger :
    ?input_mvar:string Lwt_mvar.t -> unit -> Script_typed_ir.logger

  val execute :
    t ->
    Script_typed_ir.step_constants ->
    script:Script.t ->
    entrypoint:Entrypoint_repr.t ->
    parameter:Script.expr ->
    logger:Script_typed_ir.logger ->
    ( (Script_interpreter.execution_result * t) * Script_typed_ir.execution_trace,
      error trace )
    result
    Lwt.t
end
