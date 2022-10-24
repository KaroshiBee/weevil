open Handler_t
module Dap_commands = Dapper.Dap_commands

module type MAKE_HANDLER = sig
  type input
  type output
  type t
  val make : t
  val handle : t -> config:config -> string -> string Lwt.t
end

module MakeHandler (H:HANDLER) : (MAKE_HANDLER with type input := H.input and type output := H.output) = struct

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
  "cancel", (module Cancel : HANDLER);
  "initialize", (module Initialize : HANDLER);
  "configurationDone", (module Configuration : HANDLER);
  "launch", (module Launch : HANDLER);
  "attach", (module Attach : HANDLER);
  "restart", (module Restart : HANDLER);
  "disconnect", (module Disconnect : HANDLER);
  "terminate", (module Terminate : HANDLER);
] |> List.to_seq
  |> Hashtbl.of_seq


let handle_exn t config message =
  let aux command =
    Hashtbl.find_opt t command
      |> fun h -> Option.bind h (fun h ->
        let module H = MakeHandler (val h : HANDLER) in
        let h = H.make in
        (* First one that doesnt error is what we want *)
        try
          Option.some @@ H.handle h ~config message
        with Js_msg.Wrong_encoder _ ->
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
