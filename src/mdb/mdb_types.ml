open Protocol
open Environment
open Alpha_context
open Environment.Error_monad

module type INTERPRETER_CFG = sig
  type t

  val make_log :
    context ->
    Script.location ->
    ('a * 's) ->
    ('a, 's) Script_typed_ir.stack_ty ->
    t

  val unparsing_mode : Script_ir_translator.unparsing_mode

  val unparse_stack :
    t ->
    (Script.expr * string option * bool) list tzresult Lwt.t

  val get_loc :
    t -> Script.location

  val get_gas :
    t -> Gas.t
end

module Log_records = struct
  module LocHashtbl = Hashtbl.Make(struct
      type t = Script.location
      let equal i j = i=j
      let hash i = i land max_int
    end)

  type 'log_element log_record = [ `New of 'log_element | `Old of 'log_element ]
  let make_new l = `New l
  let make_old l = `Old l

  type 'log_element t = 'log_element log_record LocHashtbl.t
  let make () = LocHashtbl.create 500

  let add_new t loc log_element =
    LocHashtbl.add t loc @@ make_new log_element

  let add_old t loc log_element =
    LocHashtbl.add t loc @@ make_old log_element

  let mem t = LocHashtbl.mem t
  let to_list t = LocHashtbl.to_seq t |> List.of_seq

  let new_to_old_inplace ~keep_old t =
    LocHashtbl.filter_map_inplace (fun _ky -> function
        | `New l -> Some (make_old l)
        | `Old _ as l -> if keep_old then Some l else None
      ) t

end


module type INTERPRETER = sig

  type t
  type log_records

  val trace_logger :
    ?full_trace:bool ->
    ?log_records:log_records ->
    in_channel:in_channel ->
    unit ->
    t

  val get_execution_trace_updates :
    t -> Script_typed_ir.execution_trace tzresult Lwt.t

  val execute :
    Alpha_context.t ->
    Script_typed_ir.step_constants ->
    script:Script.t ->
    entrypoint:Entrypoint_repr.t ->
    parameter:Script.expr ->
    logger:t ->
    (Script_interpreter.execution_result * Alpha_context.t) tzresult Lwt.t

end
