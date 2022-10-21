open Handler_t


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
