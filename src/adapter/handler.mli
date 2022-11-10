type t

val make : t

(* NOTE this is subtle,
   we want to turn all messages that need to go back to the front end into strings,
   so happy-path messages *and* error message responses,
   they live in the type 'a = string list of the ('a, _) Result.t,

   however we can still have errors (e.g. not being able to decode the incoming message at all)
   which we just want to log (for now) and not respond to,
   that is the type 'b = string of the (_, 'b) Result.t

   TODO return these arb errors as ErrorResponse strs too, will need correct seqr somehow?

*)
val handle_exn :
  t ->
  Dapper.Dap_config.t ->
  string ->
  (string list, string) Lwt_result.t
