open Protocol
open Alpha_context

module Mich_config = struct
  type t = {
    script_filename:string; storage:string; parameter:string; entrypoint:string;
  }

  let make ~script_filename ~storage ~parameter ~entrypoint () =
    {script_filename; storage; parameter; entrypoint }

  let enc =
    let open Data_encoding in
    conv
      (fun {script_filename; storage; parameter; entrypoint;} -> (script_filename, storage, parameter, entrypoint ))
      (fun (script_filename, storage, parameter, entrypoint ) -> {script_filename; storage; parameter; entrypoint })
      (obj4
         (req "script_filename" string)
         (req "storage" string)
         (req "parameter" string)
         (req "entrypoint" string)
      )

end


module type INTERPRETER_CFG = sig
  open Environment
  open Environment.Error_monad

  type t

  val make_log :
    context ->
    Tezos_micheline.Micheline_parser.location ->
    ('a * 's) ->
    ('a, 's) Script_typed_ir.stack_ty ->
    t

  val unparsing_mode : Script_ir_translator.unparsing_mode

  val unparse_stack :
    t ->
    (Script.expr * string option * bool) list tzresult Lwt.t

  val get_loc :
    t -> Tezos_micheline.Micheline_parser.location

  val get_gas :
    t -> Gas.t
end


module type INTERPRETER = sig

  open Environment
  open Environment.Error_monad

  type t

  val trace_interp :
    in_channel:in_channel ->
    out_channel:out_channel ->
    Mdb_file_locations.t ->
    t

  val execute :
    Alpha_context.t ->
    Script_typed_ir.step_constants ->
    script:Script.t ->
    entrypoint:Entrypoint_repr.t ->
    parameter:Script.expr ->
    interp:t ->
    (Script_interpreter.execution_result * Alpha_context.t) tzresult Lwt.t

end


module type STEPPER = sig

  type t
  type interp

  val code_trace : t -> (
      Script.expr *
      Apply_internal_results.packed_internal_contents trace *
      Lazy_storage.diffs option
    ) option

  val chain_id : t -> Chain_id.t
  val alpha_context : t ->  Alpha_context.t
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
    (Mdb_typechecker.t * Mdb_typechecker.t * Mdb_typechecker.t * Entrypoint.t) tzresult Lwt.t

  val step :
    make_interp:(Mdb_file_locations.t -> interp) ->
    script:Mdb_typechecker.t ->
    storage:Mdb_typechecker.t ->
    input:Mdb_typechecker.t ->
    entrypoint:Entrypoint.t ->
    t ->
    t tzresult Lwt.t

end
