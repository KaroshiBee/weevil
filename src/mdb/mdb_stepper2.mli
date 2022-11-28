open Protocol
open Alpha_context

type ctxt
type code_trace
type t

val originate_dummy_contract :
  ctxt ->
  Script.t ->
  Tez.t ->
  (ctxt * Contract_hash.t, error trace) result Lwt.t

val configure_contracts :
  ctxt ->
  Script.t ->
  Tez.t option ->
  src_opt:Contract.t option ->
  pay_opt:Contract.t option ->
  self_opt:Contract_hash.t option ->
  (ctxt * Tezos_protocol_plugin_014_PtKathma.RPC.Scripts.run_code_config, error trace) result Lwt.t

val trace_code :
  ?unparsing_mode:Script_ir_translator.unparsing_mode ->
  ?gas:Gas.Arith.integral ->
  ?entrypoint:Entrypoint_repr.t ->
  ?balance:Tez.t ->
  script:Script.expr ->
  storage:Script.expr ->
  input:Script.expr ->
  ?amount:Tez.t ->
  ?chain_id:Chain_id.t ->
  ?source:Contract.t ->
  ?payer:Contract.t ->
  ?self:Contract_hash.t ->
  ?now:Script_timestamp.t ->
  ?level:Script_int.n Script_int.num ->
  input_mvar:string Lwt_mvar.t ->
  ctxt ->
  (code_trace, error trace) result Lwt.t

val process :
  ?protocol_str:string ->
  ?base_dir:string ->
  ?input_mvar:'a ->
  ?output_mvar:'b ->
  ?_headless:bool ->
  string ->
  (t, error trace) result Lwt.t
