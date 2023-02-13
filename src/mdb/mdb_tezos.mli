module type Protocol = sig

  module type Alpha_context = sig

    type context

    module type Script = sig
      type expr
    end

    module type Gas = sig
      type t
    end

  end (* alpha context *)

  module type Script_typed_ir = sig
    type stack_ty
  end

  module type Script_ir_translator = sig
    type unparsing_mode
  end

end

module type Environment = sig

  module type Error_monad = sig
    type 'a tzresult
  end

end
