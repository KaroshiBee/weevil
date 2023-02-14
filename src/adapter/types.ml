module Dap = Dapper.Dap
module Model = Mdb.Mdb_model

(* readonly wrt the adapter level state,
   Dap.STATE_T is always needed and that modifies Dapper state *)
module type STATE_READONLY_T = sig
  (* sequencing is controlled by Dap_state in dapper lib *)
  include Dap.STATE_T

  (* the backend svc process, using process_none to allow for std redirection if needed later on *)
  val backend_svc : t -> Lwt_process.process_none option

  (* the backend comms channels *)
  val backend_ic : t -> Lwt_io.input_channel option
  val backend_oc : t -> Lwt_io.output_channel option
  (* the backend data that gets sent back up to the adapter *)
  val log_records : t -> Model.Weevil_json.t list

  (* whether to restart when terminating  *)
  val should_restart_on_terminate : t -> bool option

  (* need to retain which launch type was requested *)
  val launch_mode : t -> Dap.Launch_mode.t option

  (* the adapter internal config data *)
  val config : t -> Dap.Config.t

  (* the config data that the client requested when initializing the adapter *)
  val client_config : t -> Dap.Data.InitializeRequestArguments.t option

  (* the mdb stuff - script, storage, parameter *)
  val mdb_config : t -> Mdb.Mdb_config.t option

end

module type STATE_T = sig
  include STATE_READONLY_T

  val reset_backend : t -> unit

  val set_start_backend : t -> Ipaddr.t -> int -> string -> unit Dap.Result.t

  val set_connect_backend : t -> Ipaddr.t -> int -> (Lwt_io.input_channel * Lwt_io.output_channel) Dap.Result.t

  val set_new_log_records : t -> Model.Weevil_json.t list -> unit

  val set_launch_mode : t -> Dap.Launch_mode.t -> unit

  val set_config : t -> Dap.Config.t -> unit

  val set_client_config : t -> Dap.Data.InitializeRequestArguments.t -> unit

  val set_mdb_config : t -> Mdb.Mdb_config.t -> unit

  val set_should_restart_on_terminate : t -> bool option -> unit

end


module type STRING_HANDLER_T = sig

  type state

  val handlers :
    state:state -> (string -> (string, string) Lwt_result.t) list

  val on_success : state:state -> unit
  val on_error : state:state -> unit


end


module type HANDLER_T = sig

  type t

  type state

  val make : t

  (* NOTE this is subtle,
     we want to turn all messages that need to go back to the front end into strings,
     so happy-path messages *and* error message responses,
     they live in the type 'a = string list of the ('a, _) Result.t,

     however we *may* still have errors
     which we also need to deal with (probably just log out to err)
     that is the type 'b = string of the (_, 'b) Result.t

     TODO return these arb errors as ErrorResponse strs too?

  *)
  val handle_exn :
    t ->
    state ->
    string ->
    (string list, string) Lwt_result.t
end
