type t
val make : t

(* NOTE the IO channels here are for the backend *)
val handle_exn :
  t ->
  Lwt_process.process_full option ->
  Dapper.Dap_config.t ->
  string ->
  (string, string) Result.t Lwt.t
