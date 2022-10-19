

(* some helper modules *)
module EmptyObject : sig
  type t
  val module_name : string
  val enc : t Data_encoding.t
  val make : unit -> t
end = struct

  let module_name = "EmptyObject"

  type t = unit

  let enc = Data_encoding.empty

  let make () = ()

end

module IntString : sig
  type t
  val module_name : string
  val enc : t Data_encoding.t
  val of_int : int -> t
  val of_string : string -> t
end = struct

  let module_name = "IntString"

  type t =
    | I of int
    | S of string


  let enc =
    let open Data_encoding in
    union [
      case ~title:"int" (Tag 0)
        int31
        (function I i -> Some i | _ -> None)
        (fun i -> I i);
      case ~title:"string" (Tag 1)
        string
        (function S s -> Some s | _ -> None)
        (fun s -> S s);
    ]

  let of_int i = I i
  let of_string s = S s

end


module RestartRequestArguments (M:sig val module_name : string end) : sig
  type t
  val module_name : string
  val enc : t Data_encoding.t
  val make : ?restart:Data_encoding.json -> unit -> t
end = struct

  (* NOTE Launch/Attach request arguments are the same types *)
  let module_name = M.module_name
  type t = {restart: Data_encoding.json option}

  let enc =
    let open Data_encoding in
    conv
      (fun {restart} -> restart)
      (fun restart -> {restart})
      (obj1
         (opt "__restart" json)
      )

  let make ?restart () =
    {restart}
end

module LaunchRequestArguments = RestartRequestArguments (struct let module_name = "LaunchRequestArguments" end)
module AttachRequestArguments = RestartRequestArguments (struct let module_name = "AttachRequestArguments" end)


module RestartArguments : sig
  type t
  val module_name : string
  val enc : t Data_encoding.t
  val of_launch_args : LaunchRequestArguments.t -> t
  val of_attach_args : AttachRequestArguments.t -> t
end = struct

  let module_name = "RestartArguments"

  type t =
    | LaunchRequestArgs of LaunchRequestArguments.t
    | AttachRequestArgs of AttachRequestArguments.t

  let enc =
    let open Data_encoding in
    union [
      case ~title:"launch" (Tag 0)
        LaunchRequestArguments.enc
        (function LaunchRequestArgs args -> Some args | _ -> None)
        (fun args -> LaunchRequestArgs args);
      case ~title:"attach" (Tag 1)
        AttachRequestArguments.enc
        (function AttachRequestArgs args -> Some args | _ -> None)
        (fun args -> AttachRequestArgs args);
    ]

  let of_launch_args args = LaunchRequestArgs args
  let of_attach_args args = AttachRequestArgs args

end

(* NOTE hard code this as it rarely changes, as noted above
   the whole protocol is based on requests, responses and events *)
module ProtocolMessage_type = struct
  type t = Request | Response | Event

  let enc =
    let open Data_encoding in
     conv_with_guard
      (function Request -> "request" | Response -> "response" | Event -> "event")
      (function "request" -> Ok Request | "response" -> Ok Response | "event" -> Ok Event
              | _ as s -> Error (Printf.sprintf "Unknown ProtocolMessage_type '%s'" s))
      string

end

module type ENC0 = sig
  type t
  val value : t
  val enc : t Data_encoding.t
end

module type PRESENCE = sig
  type req
  type opt
end
