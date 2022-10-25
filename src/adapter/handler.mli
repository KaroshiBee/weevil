type t
val make : t
val handle_exn : t -> Dapper.Dap_handler_t.config -> string -> (string, string) Result.t Lwt.t
val main_handler :
  t ->
  Dapper.Dap_handler_t.config ->
  content_length:int option ->
  Conduit_lwt_unix.flow ->
  Lwt_io.input_channel ->
  Lwt_io.output_channel ->
  unit Lwt.t
