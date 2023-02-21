module T : functor (Stepper : Mdb_types.STEPPER_T) -> sig
  val cmd : unit Cmdliner.Term.t -> unit Cmdliner.Cmd.t
end
