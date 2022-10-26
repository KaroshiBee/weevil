type t
val make : t

(* NOTE the IO channels here are for the backend *)
val handle_exn :
  t ->
  (Lwt_io.input_channel * Lwt_io.output_channel) ->
  Dapper.Dap_config.t ->
  string ->
  (string, string) Result.t Lwt.t

val main_handler :
  t ->
  Dapper.Dap_config.t ->
  int option ->
  Conduit_lwt_unix.flow ->
  Lwt_io.input_channel ->
  Lwt_io.output_channel ->
  unit Lwt.t
