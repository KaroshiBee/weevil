(* for testing *)
module T : functor (S:Types.State_intf) -> Types.Handler_intf with type state := S.t
