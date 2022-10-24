open Handler_t
module Dap_commands = Dapper.Dap_commands

module type MAKE_HANDLER = sig
  type input
  type output
  type t
  val make : t
  val handle : t -> config:config -> string -> string Lwt.t
end

module Handler (H:HANDLER) : (MAKE_HANDLER with type input := H.input and type output := H.output) = struct

  type t = {
    from_string : string -> H.input;
    to_string : H.output -> (string, string) Result.t Lwt.t ;
    handle : config:config -> H.input -> H.output Lwt.t;
  }

  let make = {
    from_string = H.from_string;
    to_string = H.to_string;
    handle = H.handle;
  }

  let handle t ~config s =
    let%lwt output =
      t.from_string s
      |> t.handle ~config
    in
    match%lwt t.to_string output with
    | Result.Ok msg ->
      Lwt.return msg
    | Result.Error err ->
      Lwt.return err


end

type t = (string, (module HANDLER)) Hashtbl.t

let make : t = [
  "Cancel", (module Cancel);
  "Initialize", (module Initialize);
  "ConfigurationDone", (module Configuration);
  "Launch", (module Launch);
  "Attach", (module Attach);
  "Restart", (module Restart);
  "Disconnect", (module Disconnect);
  "Terminate", (module Terminate);
] |> List.map (fun (name, m) ->
    let module M = (val m : HANDLER) in
    let key = String.uncapitalize_ascii name in
    (key, m)
  )
  |> List.to_seq
  |> Hashtbl.of_seq


let handle_exn t config message =
  let aux command =
    Hashtbl.find_opt t command
      |> fun h -> Option.bind h (fun h ->
        let module H = Handler (val h : HANDLER) in
        let h = H.make in
        try
          Option.some @@ H.handle h ~config message
        with _ ->
          None
      )
  in
  ["cancel";
   "initialize";
   "configurationDone";
   "launch";
   "attach";
   "restart";
   "disconnect";
   "terminate";
  ] |> List.find_map aux |> Option.get
