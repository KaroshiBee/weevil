open Mdb_import.Tez

type t
type cfg_t


(* old cfg *)
val make_log :
  Ctxt.context ->
  Tezos_micheline.Micheline_parser.location ->
  ('a * 's) ->
  ('a, 's) Prot.Script_typed_ir.stack_ty ->
  cfg_t

(* old cfg *)
val unparsing_mode :
  Prot.Script_ir_unparser.unparsing_mode

(* old cfg *)
(* TODO why err.tzresult? *)
val unparse_stack :
  cfg_t ->
  (Ctxt.Script.expr * string option * bool) list Err.tzresult Lwt.t

(* old cfg *)
val get_loc :
  cfg_t -> Tezos_micheline.Micheline_parser.location

(* old cfg *)
val get_gas :
  cfg_t -> Ctxt.Gas.t

(* old interp.trace_interp *)
val make :
  in_channel:in_channel ->
  out_channel:out_channel ->
  Mdb_michelson.File_locations.locs ->
  t

(* old interp.execute without interp ctor function *)
(* TODO why err.tzresult? *)
val execute :
  Ctxt.context ->
  Prot.Script_typed_ir.step_constants ->
  script:Ctxt.Script.t ->
  entrypoint:Ctxt.Entrypoint.t ->
  parameter:Ctxt.Script.expr ->
  interp:t ->
  (Prot.Script_interpreter.execution_result * Ctxt.context) Err.tzresult Lwt.t
