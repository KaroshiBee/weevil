type t

val process :
  ?protocol_str:string ->
  ?base_dir:string ->
  ?input_mvar:'input Lwt_mvar.t ->
  ?output_mvar:'output Lwt_mvar.t ->
  ?_headless:bool ->
  string ->
  (t, error trace) result Lwt.t
