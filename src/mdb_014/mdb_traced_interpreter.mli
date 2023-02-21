module Tez = Import.Tez

type t
type cfg_t


(* old cfg *)
val make_log :
  Tez.Ctxt.context ->
  Tezos_micheline.Micheline_parser.location ->
  ('a * 's) ->
  ('a, 's) Tez.Prot.Script_typed_ir.stack_ty ->
  cfg_t

(* old cfg *)
val unparsing_mode :
  Tez.Prot.Script_ir_translator.unparsing_mode

(* old cfg *)
(* TODO why Tez.err.tzresult? *)
val unparse_stack :
  cfg_t ->
  (Tez.Ctxt.Script.expr * string option * bool) list Tez.Err.tzresult Lwt.t

(* old cfg *)
val get_loc :
  cfg_t -> Tezos_micheline.Micheline_parser.location

(* old cfg *)
val get_gas :
  cfg_t -> Tez.Ctxt.Gas.t

(* old interp.trace_interp *)
val make :
  in_channel:in_channel ->
  out_channel:out_channel ->
  Michelson.File_locations.t ->
  t

(* old interp.execute without interp ctor function *)
(* TODO why Tez.err.tzresult? *)
val execute :
  Tez.Ctxt.context ->
  Tez.Prot.Script_typed_ir.step_constants ->
  script:Tez.Ctxt.Script.t ->
  entrypoint:Tez.Ctxt.Entrypoint.t ->
  parameter:Tez.Ctxt.Script.expr ->
  interp:t ->
  (Tez.Prot.Script_interpreter.execution_result * Tez.Ctxt.context) Tez.Err.tzresult Lwt.t
