type t
val make : t
val handle_exn : t -> Handler_t.config -> string -> (string, string) Result.t Lwt.t
