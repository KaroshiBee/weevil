(* for testing *)
module T : functor (S:Types.State_readonly_intf) -> Types.String_handler_intf with type state := S.t
