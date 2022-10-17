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
   these make a phantom'd (_, _, req) & (_, _, opt) resp.

   This then allows a uniform API of three main messaging modules and sigs:
   * ('command, 'args, 'presence) RequestMessage.t
   * ('command, 'body, 'presence) ResponseMessage.t
   * ('event, 'body, 'presence) EventMessage.t

   Finally, in the autogenerated Dap_message module, we define three sum types:
   * request
   * response
   * event
   with constructors named after the various message objects defined in the json schema.

   NOTE the autogeneration takes care of assigning the correct phantoms for command/event and req/opt
   and so compiler errors should abound if one uses the wrong ctor function.

*)


(* some helper modules *)
module EmptyObject = struct

  let module_name = "EmptyObject"

  type t = unit

  let enc = Data_encoding.empty

  let make () = ()

end

module IntString = struct

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


module RestartRequestArguments (M:sig val module_name : string end) = struct
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


module RestartArguments = struct
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

module type REQUEST = sig

  include PRESENCE

  type ('cmd, 'args, 'presence) t

  val seq : ('cmd, 'args, 'presence) t -> int
  val incr : ('cmd, 'args, 'presence) t -> ('cmd, 'args, 'presence) t
  val message : ('cmd, 'args, 'presence) t -> ProtocolMessage_type.t
  val command : ('cmd, 'args, 'presence) t -> 'cmd Dap_command.t
  val arguments : ('cmd, 'args, 'presence) t -> 'args
  val enc :     'args Data_encoding.t -> ('cmd, 'args, req) t Data_encoding.t
  val enc_opt : 'args Data_encoding.t -> ('cmd, 'args option, opt) t Data_encoding.t
  val make :     seq:int -> command:'cmd Dap_command.t ->  arguments:'args -> unit -> ('cmd, 'args, req) t
  val make_opt : seq:int -> command:'cmd Dap_command.t -> ?arguments:'args -> unit -> ('cmd, 'args option, opt) t
end

module RequestMessage : REQUEST = struct

  type req
  type opt

  type ('cmd, 'args, 'presence) t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    command : 'cmd Dap_command.t;
    arguments : 'args;
  }

  let seq t = t.seq
  let incr t = {t with seq=succ @@ seq t}
  let message t = t.type_
  let command t = t.command
  let arguments t = t.arguments

  let enc args =
    let open Data_encoding in
    conv
      (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
      (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
      (obj4
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "command" Dap_command.enc)
         (req "arguments" args))

  let enc_opt args =
    let open Data_encoding in
    conv
      (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
      (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
      (obj4
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "command" Dap_command.enc)
         (opt "arguments" args))

  let make ~seq ~command ~arguments () =
    let type_ = ProtocolMessage_type.Request in
    {seq; type_; command; arguments}

  let make_opt ~seq ~command ?arguments () =
    let type_ = ProtocolMessage_type.Request in
    {seq; type_; command; arguments}
end


module type RESPONSE = sig

  include PRESENCE

  type ('cmd, 'body, 'presence) t

  val seq : ('cmd, 'body, 'presence) t -> int
  val set_seq : ('cmd, 'body, 'presence) t -> int ->  ('cmd, 'body, 'presence) t
  val incr : ('cmd, 'body, 'presence) t -> ('cmd, 'body, 'presence) t
  val type_ : ('cmd, 'body, 'presence) t -> ProtocolMessage_type.t
  val request_seq : ('cmd, 'body, 'presence) t -> int
  val set_request_seq : ('cmd, 'body, 'presence) t -> int ->  ('cmd, 'body, 'presence) t
  val success : ('cmd, 'body, 'presence) t -> bool
  val command : ('cmd, 'body, 'presence) t -> 'cmd Dap_command.t
  val message : ('cmd, 'body, 'presence) t -> string option
  val body : ('cmd, 'body, 'presence) t -> 'body
  val enc :     'body Data_encoding.t -> ('cmd, 'body, req) t Data_encoding.t
  val enc_opt : 'body Data_encoding.t -> ('cmd, 'body option, opt) t Data_encoding.t
  val make :     seq:int -> request_seq:int -> success:bool -> command:'cmd Dap_command.t -> ?message:string ->  body:'body -> unit -> ('cmd, 'body, req) t
  val make_opt : seq:int -> request_seq:int -> success:bool -> command:'cmd Dap_command.t -> ?message:string -> ?body:'body -> unit -> ('cmd, 'body option, opt) t
end

module ResponseMessage : RESPONSE = struct

  type req
  type opt

  type ('cmd, 'body, 'presence) t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    request_seq : int;
    success : bool;
    command : 'cmd Dap_command.t;
    message : string option; (* only used once I think, keep as string for now *)
    body : 'body;
  }

  let seq t = t.seq
  let set_seq t seq = {t with seq}
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
  let request_seq t = t.request_seq
  let set_request_seq t request_seq = {t with request_seq}
  let success t = t.success
  let command t = t.command
  let message t = t.message
  let body t = t.body

  let enc body =
    let open Data_encoding in
    conv
      (fun {seq; type_; request_seq; success; command; message; body} ->
        (seq, type_, request_seq, success, command, message, body))
      (fun (seq, type_, request_seq, success, command, message, body) ->
        {seq; type_; request_seq; success; command; message; body})
      (obj7
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "request_seq" int31)
         (req "success" bool)
         (req "command" Dap_command.enc)
         (opt "message" string)
         (req "body" body))

  let enc_opt body =
    let open Data_encoding in
    conv
      (fun {seq; type_; request_seq; success; command; message; body} ->
        (seq, type_, request_seq, success, command, message, body))
      (fun (seq, type_, request_seq, success, command, message, body) ->
        {seq; type_; request_seq; success; command; message; body})
      (obj7
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "request_seq" int31)
         (req "success" bool)
         (req "command" Dap_command.enc)
         (opt "message" string)
         (opt "body" body))

  let make ~seq ~request_seq ~success ~command ?message ~body () =
    let type_ = ProtocolMessage_type.Response in
    {seq; type_; request_seq; success; command; message; body}

  let make_opt ~seq ~request_seq ~success ~command ?message ?body () =
    let type_ = ProtocolMessage_type.Response in
    {seq; type_; request_seq; success; command; message; body}

end

module type EVENT = sig

  include PRESENCE

  type ('event, 'body, 'presence) t

  val seq : ('event, 'body, 'presence) t -> int
  val set_seq : ('event, 'body, 'presence) t -> int -> ('event, 'body, 'presence) t
  val incr : ('event, 'body, 'presence) t -> ('event, 'body, 'presence) t
  val type_ : ('event, 'body, 'presence) t -> ProtocolMessage_type.t
  val event : ('event, 'body, 'presence) t -> 'event Dap_event.t
  val body : ('event, 'body, 'presence) t -> 'body
  val enc :     'body Data_encoding.t -> ('event, 'body, req) t Data_encoding.t
  val enc_opt : 'body Data_encoding.t -> ('event, 'body option, opt) t Data_encoding.t
  val make :     seq:int -> event:'event Dap_event.t ->  body:'body -> unit -> ('event, 'body, req) t
  val make_opt : seq:int -> event:'event Dap_event.t -> ?body:'body -> unit -> ('event, 'body option, opt) t
end

module EventMessage : EVENT = struct

  type req
  type opt

  type ('event, 'body, 'presence) t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    event : 'event Dap_event.t;
    body : 'body;
  }

  let seq t = t.seq
  let set_seq t seq = {t with seq}
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
  let event t = t.event
  let body t = t.body

  let enc body =
    let open Data_encoding in
    conv
      (fun {seq; type_; event; body} -> (seq, type_, event, body))
      (fun (seq, type_, event, body) -> {seq; type_; event; body})
      (obj4
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "event" Dap_event.enc)
         (req "body" body))

  let enc_opt body =
    let open Data_encoding in
    conv
      (fun {seq; type_; event; body} -> (seq, type_, event, body))
      (fun (seq, type_, event, body) -> {seq; type_; event; body})
      (obj4
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "event" Dap_event.enc)
         (opt "body" body))

  let make ~seq ~event ~body () =
    let type_ = ProtocolMessage_type.Event in
    {seq; type_; event; body}

  let make_opt ~seq ~event ?body () =
    let type_ = ProtocolMessage_type.Event in
    {seq; type_; event; body}

end