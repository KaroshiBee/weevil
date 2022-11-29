open Protocol
open Alpha_context

module T : functor (INTERP:Mdb_types.INTERPRETER) -> sig

type t

val code_trace : t -> (Script.expr * Apply_internal_results.packed_internal_contents trace * Script_typed_ir.execution_trace * Lazy_storage.diffs option) option
val chain_id : t -> Chain_id.t
val alpha_context : t ->  Alpha_context.t
val mock_context : t -> Tezos_client_base_unix.Client_context_unix.unix_mockup

val init :
  protocol_str:string ->
  base_dir:string ->
  unit ->
  (t, error trace) result Lwt.t

val typecheck :
  t ->
  string ->
  (Mdb_typechecker.t, error trace) result Lwt.t

val process :
  make_logger:(unit -> INTERP.logger) ->
  t ->
  string ->
  (t, error trace) result Lwt.t

end
