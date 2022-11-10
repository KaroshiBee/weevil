type t

val make : t

val handle_exn :
  t ->
  Dapper.Dap_config.t ->
  string ->
  (string, string) Lwt_result.t
