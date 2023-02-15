module T (Ctxt:Mdb_types.CTXT) : sig

  type t

  val setup_mockup_rpc_client_config :
    base_dir:string ->
    Tezos_client_base.Client_context.printer ->
    Tezos_crypto.Protocol_hash.t option ->
    t tzresult Lwt.t

  val chain_id : t -> Tezos_crypto.Chain_id.t
  val mock_context : t -> Tezos_client_base_unix.Client_context_unix.unix_mockup
  val make_alpha_context : t -> Ctxt.t tzresult Lwt.t

end
