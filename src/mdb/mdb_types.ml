module type PROT = module type of Tezos_protocol_014_PtKathma.Protocol
module type CTXT = module type of Tezos_protocol_014_PtKathma.Protocol.Alpha_context
module type ENV  = module type of Tezos_protocol_014_PtKathma.Environment
module type ERR  = module type of Tezos_protocol_014_PtKathma.Environment.Error_monad

module MakeTezos (P:PROT) (Env:ENV) = struct

  module Ctxt = P.Alpha_context
  module Err = Env.Error_monad

  type 'a tzresult = 'a Err.tzresult

  type ('a, 'b) ty = ('a, 'b) P.Script_typed_ir.ty
  type ('a, 's) stack_ty = ('a, 's) P.Script_typed_ir.stack_ty
  type step_constants = P.Script_typed_ir.step_constants
  type execution_result = P.Script_interpreter.execution_result
  type packed_internal_contents = P.Apply_internal_results.packed_internal_contents
  type unparsing_mode = P.Script_ir_translator.unparsing_mode

  type context = Ctxt.context
  type expr = Ctxt.Script.expr
  type gas = Ctxt.Gas.t
  type entrypoint = Ctxt.Entrypoint.t
  type script = Ctxt.Script.t
  type diffs = Ctxt.Lazy_storage.diffs
end

module Tez014 = MakeTezos
    (Tezos_protocol_014_PtKathma.Protocol)
    (Tezos_protocol_014_PtKathma.Environment)

module type INTERPRETER_CFG = sig

  module P = Tezos_protocol_014_PtKathma.Protocol
  module Ctxt = P.Alpha_context
  module Env = Tezos_protocol_014_PtKathma.Environment
  module Err = Tezos_protocol_014_PtKathma.Environment.Error_monad

  type t

  val make_log :
    Ctxt.context ->
    Tezos_micheline.Micheline_parser.location ->
    ('a * 's) ->
    ('a, 's) P.Script_typed_ir.stack_ty ->
    t

  val unparsing_mode :
    P.Script_ir_translator.unparsing_mode

  val unparse_stack :
    t ->
    (Ctxt.Script.expr * string option * bool) list Err.tzresult Lwt.t

  val get_loc :
    t -> Tezos_micheline.Micheline_parser.location

  val get_gas :
    t -> Ctxt.Gas.t

end

module type INTERPRETER_CFG_T = sig

  type t

  val make_log :
    Tez014.context ->
    Tezos_micheline.Micheline_parser.location ->
    ('a * 's) ->
    ('a, 's) Tez014.stack_ty ->
    t

  val unparsing_mode : Tez014.unparsing_mode

  val unparse_stack :
    t ->
    (Tez014.expr * string option * bool) list Tez014.tzresult Lwt.t

  val get_loc :
    t -> Tezos_micheline.Micheline_parser.location

  val get_gas :
    t -> Tez014.gas

end

module type INTERPRETER = sig

  module P = Tezos_protocol_014_PtKathma.Protocol
  module Ctxt = P.Alpha_context
  module Env = Tezos_protocol_014_PtKathma.Environment
  module Err = Tezos_protocol_014_PtKathma.Environment.Error_monad

  type t

  val trace_interp :
    in_channel:in_channel ->
    out_channel:out_channel ->
    Mdb_file_locations.t ->
    t

  val execute :
    Ctxt.t ->
    P.Script_typed_ir.step_constants ->
    script:Ctxt.Script.t ->
    entrypoint:Ctxt.Entrypoint.t ->
    parameter:Ctxt.Script.expr ->
    interp:t ->
    (P.Script_interpreter.execution_result * Ctxt.t) Err.tzresult Lwt.t

end

module type INTERPRETER_T = sig

  type t

  val trace_interp :
    in_channel:in_channel ->
    out_channel:out_channel ->
    Mdb_file_locations.t ->
    t

  val execute :
    Tez014.context ->
    Tez014.step_constants ->
    script:Tez014.script ->
    entrypoint:Tez014.entrypoint ->
    parameter:Tez014.expr ->
    interp:t ->
    (Tez014.execution_result * Tez014.context) Tez014.tzresult Lwt.t

end

module type STEPPER = sig

  module P = Tezos_protocol_014_PtKathma.Protocol
  module Ctxt = P.Alpha_context

  type t
  type interp

  val code_trace : t -> (
      Ctxt.Script.expr *
      P.Apply_internal_results.packed_internal_contents trace *
      Ctxt.Lazy_storage.diffs option
    ) option

  val chain_id : t -> Tezos_crypto.Chain_id.t
  val alpha_context : t -> Ctxt.t
  val mock_context : t -> Tezos_client_base_unix.Client_context_unix.unix_mockup

  val init :
    protocol_str:string ->
    base_dir:string ->
    unit ->
    t tzresult Lwt.t

  val typecheck :
    script_filename:string ->
    storage:string ->
    input:string ->
    entrypoint:string ->
    t ->
    (Mdb_typechecker.t * Mdb_typechecker.t * Mdb_typechecker.t * Ctxt.Entrypoint.t) tzresult Lwt.t

  val step :
    make_interp:(Mdb_file_locations.t -> interp) ->
    script:Mdb_typechecker.t ->
    storage:Mdb_typechecker.t ->
    input:Mdb_typechecker.t ->
    entrypoint:Ctxt.Entrypoint.t ->
    t ->
    t tzresult Lwt.t

end


module type STEPPER_T = sig

  type interp
  type t

  val code_trace : t -> (
      Tez014.expr *
      Tez014.packed_internal_contents list *
      Tez014.diffs option
    ) option

  val chain_id : t -> Tezos_crypto.Chain_id.t
  val alpha_context : t -> Tez014.context
  val mock_context : t -> Tezos_client_base_unix.Client_context_unix.unix_mockup

  val init :
    protocol_str:string ->
    base_dir:string ->
    unit ->
    t Tez014.tzresult Lwt.t

  val typecheck :
    script_filename:string ->
    storage:string ->
    input:string ->
    entrypoint:string ->
    t ->
    (Mdb_typechecker.t * Mdb_typechecker.t * Mdb_typechecker.t * Tez014.entrypoint) Tez014.tzresult Lwt.t

  val step :
    make_interp:(Mdb_file_locations.t -> interp) ->
    script:Mdb_typechecker.t ->
    storage:Mdb_typechecker.t ->
    input:Mdb_typechecker.t ->
    entrypoint:Tez014.entrypoint ->
    t ->
    t Tez014.tzresult Lwt.t

end
