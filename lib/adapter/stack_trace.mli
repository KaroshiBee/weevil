(* for testing *)
module T : functor (S:Types.STATE_T) -> Types.STRING_HANDLER_T with type state := S.t
