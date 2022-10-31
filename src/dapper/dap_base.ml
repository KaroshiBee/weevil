(* there are three message types: Request, Response, Event
   they all three have to conform to the ProtocolMessage spec
   which is having fields "seq" & "type",
   they also have their own additional specs,
   all types are auto-generated (enums, plain objects, Request/Response/Event message types etc)

   NOTE According to the spec

   - there are pairings that have to be observed (ie a NextResponse is a response to a NextRequest)

   - The arguments field of Requests can sometimes be optional,

   - The body field of Event and Response can sometimes be optional

   - The message field of Response is supposed to be an _enum
      but the spec only specifies "cancelled" as a suggestion
      and no-where is it used, so we keep it as a string option

   All Request/Response/Event message types are parameterised by three type parameters,
   the first type param is a phantom type for stating what kind of
   Request/Response it is (based on the Dap_command types) or what kind of
   Event it is (based on the Dap_event types).
   NOTE that Dap_command and Dap_event are auto-generated from the DAP schema.
   This first type param can be used to control pairings of request/responses.

   The second type param is just the type for the args (for requests) or body (for events and responses).

   The third type param is another phantom that allows to specify the kind of presence for the args/body,
   we use the name 'presence (which is a type req or opt)
   and define two sets of constructors:
   * enc & enc_opt - for constructing from json
   * make & make_opt for constructing in ocaml
   these make a phantom'd (_, _, req) & (_, _, opt) respectively.

   This then allows a uniform API of three main messaging modules and sigs:
   * ('command, 'args, 'presence) RequestMessage.t
   * ('command, 'body, 'presence) ResponseMessage.t
   * ('event, 'body, 'presence) EventMessage.t

   Finally, in the autogenerated Dap_message module, we define three GADT families with the shapes:
   * CtorsForRequests : ('command, 'args, 'presence) RequestMessage.t -> ('command, 'args, 'presence) request
   * CtorsForResponses : ('command, 'body, 'presence) ResponseMessage.t -> ('command, 'body, 'presence) response
   * CtorsForEvents : ('event, 'body, 'presence) EventMessage.t -> ('event, 'body, 'presence) event

   with constructors named after the various message objects defined in the json schema.

   NOTE the autogeneration takes care of assigning the correct phantoms for command/event and req/opt
   and so compiler errors should abound if one uses the wrong ctor function.

*)

type launch_mode = [`Launch | `Attach | `AttachForSuspendedLaunch ]


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


module LaunchRequestArguments : sig
  type t
  val module_name : string
  val enc : t Data_encoding.t
  val make : ?restart:Data_encoding.json -> ?noDebug:bool -> unit -> t
  val restart : t -> Data_encoding.json option
  val noDebug : t -> bool option
end = struct

  let module_name = "LaunchRequestArguments"
  type t = {restart: Data_encoding.json option; noDebug: bool option}

  let enc =
    let open Data_encoding in
    conv
      (fun {restart; noDebug} -> (restart, noDebug))
      (fun (restart, noDebug) -> {restart; noDebug})
      (obj2
         (opt "__restart" json)
         (opt "noDebug" bool)
      )

  let make ?restart ?noDebug () =
    {restart; noDebug}

  let restart t = t.restart
  let noDebug t = t.noDebug

end

module AttachRequestArguments : sig
  type t
  val module_name : string
  val enc : t Data_encoding.t
  val make : ?restart:Data_encoding.json -> unit -> t
  val restart : t -> Data_encoding.json option
end = struct

  let module_name = "AttachRequestArguments"
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

  let restart t = t.restart

end

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
