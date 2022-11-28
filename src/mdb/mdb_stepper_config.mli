module Client_context = Tezos_client_base.Client_context
module Client_context_unix = Tezos_client_base_unix.Client_context_unix

type t = {
  chain_id:Chain_id.t;
  rpc_context:Environment.Updater.rpc_context;
  unix_mockup:Client_context_unix.unix_mockup;
}

val setup_mockup_rpc_client_config :
  Client_context.printer ->
  Protocol_hash.t option ->
  string ->
  t tzresult Lwt.t
