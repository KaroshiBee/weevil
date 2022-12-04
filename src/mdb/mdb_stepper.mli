open Protocol
open Alpha_context

module T : functor (INTERP:Mdb_types.INTERPRETER) -> sig

  type t

  val code_trace : t -> (
      Script.expr *
      Apply_internal_results.packed_internal_contents trace *
      Lazy_storage.diffs option
    ) option

  val chain_id : t -> Chain_id.t
  val alpha_context : t ->  Alpha_context.t
  val mock_context : t -> Tezos_client_base_unix.Client_context_unix.unix_mockup

  val init :
    protocol_str:string ->
    base_dir:string ->
    unit ->
    t tzresult Lwt.t

  val typecheck :
    script_filename:string ->
    t ->
    Mdb_typechecker.t tzresult Lwt.t

  val step :
    logger:INTERP.t ->
    script:Mdb_typechecker.t ->
    t ->
    t tzresult Lwt.t

end
