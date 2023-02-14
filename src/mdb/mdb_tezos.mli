module Environment : sig

  module Error_monad : sig
    type 'a tzresult
    type error = ..
    type 'a trace
    val pp_trace : Format.formatter -> error trace -> unit
  end

  module List : sig
    type 'a t = 'a list = [] | (::) of 'a * 'a list (**)
    val map : ('a -> 'b) -> 'a list -> 'b list
  end

end (* environment *)

module Protocol : sig

  module Alpha_context : sig

    type t
    type context = t

    module Script : sig
      type t
      type expr
    end

    module Gas : sig
      type t
      val pp : Format.formatter -> t -> unit
    end

  end (* alpha context *)

  module Entrypoint : sig
    type t
  end

  module Script_repr : sig
    type expr
  end

  module Script_typed_ir : sig
    type stack_ty
    type logger
  end

  module Script_ir_translator : sig
    type unparsing_mode
    type ex_script
  end

  module Script_interpreter : sig

    type step_constants
    type execution_result

    val execute :
      ?logger:Script_typed_ir.logger ->
      Alpha_context.t ->
      cached_script:Script_ir_translator.ex_script option ->
      Script_ir_translator.unparsing_mode ->
      step_constants ->
      script:Alpha_context.Script.t ->
      entrypoint:Entrypoint.t ->
      parameter:Alpha_context.Script.expr ->
      internal:bool ->
      (execution_result * Alpha_context.context) Environment.Error_monad.tzresult Lwt.t
  end

end (* protocol *)

module Plugin : sig

  module Plugin_errors : sig
    type Environment.Error_monad.error += Cannot_serialize_log
  end

end (* plugin *)

module Client : sig
  module Michelson_v1_printer : sig
    val print_expr : Format.formatter -> Protocol.Script_repr.expr -> unit
  end

end
