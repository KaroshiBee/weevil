module Client_context = Tezos_client_base.Client_context
module Client_context_unix = Tezos_client_base_unix.Client_context_unix

module P = Tezos_protocol_014_PtKathma.Protocol
(* module Ctxt = P.Alpha_context *)
module Env = Tezos_protocol_014_PtKathma.Environment

module T (Ctxt:Mdb_tezos.CTXT) = struct

  type t = {
    chain_id:Chain_id.t;
    rpc_context:Env.Updater.rpc_context;
    unix_mockup:Client_context_unix.unix_mockup;
  }

  let chain_id t = t.chain_id
  let mock_context t = t.unix_mockup
  let make_alpha_context t =
    let open Lwt_result_syntax in
    let timestamp = t.rpc_context.block_header.timestamp in
    let level = Int32.succ t.rpc_context.block_header.level in (* `Successor_level is safer? *)
    let* (alpha_context, _, _) =
      Ctxt.prepare
        ~level
        ~predecessor_timestamp:timestamp
        ~timestamp
        t.rpc_context.context
      |> Lwt.map Env.wrap_tzresult
    in
    return alpha_context

  (* NOTE we are in Tezos_base.TzPervasives.tzresult Lwt.t because of Tezos_mockup lib calls *)
  let setup_mockup_rpc_client_config ~base_dir cctxt protocol_hash =

    let open Lwt_result_syntax in

    let in_memory_mockup (protocol : Protocol_hash.t option) =
      match protocol with
      | None -> Tezos_mockup.Persistence.default_mockup_context cctxt
      | Some protocol_hash ->
        let*! () = Logs_lwt.debug (fun m -> m "making with a protocol hash") in
        Tezos_mockup.Persistence.init_mockup_context_by_protocol_hash
          ~cctxt
          ~protocol_hash
          ~constants_overrides_json:None
          ~bootstrap_accounts_json:None
    in

    let* b = Tezos_mockup.Persistence.classify_base_dir base_dir in
    let* (mockup_env, {chain = chain_id; rpc_context; protocol_data}), mem_only =
      match b with
      | Tezos_mockup.Persistence.Base_dir_is_empty
      | Tezos_mockup.Persistence.Base_dir_is_file
      | Tezos_mockup.Persistence.Base_dir_is_nonempty
      | Tezos_mockup.Persistence.Base_dir_does_not_exist ->
        let mem_only = true in
        let* res = in_memory_mockup protocol_hash in
        return (res, mem_only)
      | Tezos_mockup.Persistence.Base_dir_is_mockup ->
        let mem_only = false in
        let* res =
          Tezos_mockup.Persistence.get_mockup_context_from_disk
            ~base_dir
            ~protocol_hash
            cctxt
        in
        return (res, mem_only)
    in
    let unix_mockup = new Client_context_unix.unix_mockup
      ~base_dir
      ~mem_only
      ~mockup_env
      ~chain_id
      ~rpc_context
      ~protocol_data
    in

    return {
      chain_id;
      rpc_context;
      unix_mockup;
    }
end
