type t
val make : t

val handle_exn :
  t ->
  Dapper.Dap_config.t ->
  string ->
  (string, string) Result.t Lwt.t
