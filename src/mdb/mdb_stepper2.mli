open Protocol
open Alpha_context

module T : functor (INTERP:Mdb_types.INTERPRETER) -> sig

type t

val code_trace : t -> (Script.expr * Apply_internal_results.packed_internal_contents trace * Script_typed_ir.execution_trace * Lazy_storage.diffs option)
val chain_id : t -> Chain_id.t
val rpc_context : t ->  Environment.Updater.rpc_context
val mock_context : t -> Tezos_client_base_unix.Client_context_unix.unix_mockup

val process :
  ?protocol_str:string ->
  ?base_dir:string ->
  logger:INTERP.logger ->
  string ->
  (t, error trace) result Lwt.t


val stepper :
  ?protocol_str:string ->
  ?base_dir:string ->
  logger:INTERP.logger ->
  string ->
  (t, error trace) result
end
