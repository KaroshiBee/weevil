module type STEPPER_T = sig
  type t
  type interp
  type well_typed

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
    well_typed tzresult Lwt.t

  (* can only get file_locations from well typed script *)
  val make_interp :
    well_typed ->
    interp tzresult Lwt.t

  val step :
    interp:interp ->
    well_typed ->
    t ->
    t tzresult Lwt.t
end
