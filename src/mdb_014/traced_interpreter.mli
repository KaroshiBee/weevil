type t
type cfg_t


(* old cfg *)
val make_log :
  Import.Tez.Ctxt.context ->
  Tezos_micheline.Micheline_parser.location ->
  ('a * 's) ->
  ('a, 's) Import.Tez.Prot.Script_typed_ir.stack_ty ->
  cfg_t

(* old cfg *)
val unparsing_mode :
  Import.Tez.Prot.Script_ir_translator.unparsing_mode

(* old cfg *)
(* TODO why Import.Tez.err.tzresult? *)
val unparse_stack :
  cfg_t ->
  (Import.Tez.Ctxt.Script.expr * string option * bool) list Import.Tez.Err.tzresult Lwt.t

(* old cfg *)
val get_loc :
  cfg_t -> Tezos_micheline.Micheline_parser.location

(* old cfg *)
val get_gas :
  cfg_t -> Import.Tez.Ctxt.Gas.t

(* old interp.trace_interp *)
val make :
  in_channel:in_channel ->
  out_channel:out_channel ->
  Michelson.File_locations.t ->
  t

(* old interp.execute without interp ctor function *)
(* TODO why Import.Tez.err.tzresult? *)
val execute :
  Import.Tez.Ctxt.context ->
  Import.Tez.Prot.Script_typed_ir.step_constants ->
  script:Import.Tez.Ctxt.Script.t ->
  entrypoint:Import.Tez.Ctxt.Entrypoint.t ->
  parameter:Import.Tez.Ctxt.Script.expr ->
  interp:t ->
  (Import.Tez.Prot.Script_interpreter.execution_result * Import.Tez.Ctxt.context) Import.Tez.Err.tzresult Lwt.t
