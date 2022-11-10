(* for testing *)
module T : functor (S:Types.State_intf) -> Types.String_handler_intf with type state = S.t
include Types.String_handler_intf
