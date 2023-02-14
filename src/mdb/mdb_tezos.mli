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

  module Micheline : sig
    type t
    (* val strip_locations : *)
  end


end (* environment *)

module Protocol : sig

  module Alpha_context : sig

    type t
    type context = t

    module Script : sig
      type t
      type expr
      type node
    end

    module Gas : sig
      type t
      val pp : Format.formatter -> t -> unit
      val set_unlimited : context -> context
      val level : context -> t
    end

    module Entrypoint : sig
      type t
    end

  end (* alpha context *)

  (* module Script_repr : sig *)
  (*   type expr *)
  (*   type node *)
  (* end *)

  module Script_typed_ir : sig
    type ('a, 's) stack_ty
    type ('a, 'b) ty
    type logger
  end

  module Script_ir_translator : sig
    type unparsing_mode
    type ex_script
    val unparse_data :
      Alpha_context.context ->
      unparsing_mode ->
      ('a, _) Script_typed_ir.ty ->
      'a ->
      (Alpha_context.Script.node * Alpha_context.context) Environment.Error_monad.tzresult Lwt.t

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
      entrypoint:Alpha_context.Entrypoint.t ->
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
    val print_expr : Format.formatter -> Protocol.Alpha_context.Script.expr -> unit
  end

end
