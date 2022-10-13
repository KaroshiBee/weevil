(* there are three message types: Request, Response, Event
   they all three have to conform to the ProtocolMessage spec
   which is having fields "seq" & "type",
   they also have their own additional specs,
   all leaf types are auto-generated (enums, plain objects etc)
   and written to Dap_enum.ml and then used in the following functors

   NOTE According to the spec
   - The arguments field of Requests can sometimes be optional,

   - The body field of Event and Response can sometimes be optional

   - The message field of Response is supposed to be an _enum
      but the spec only specifies "cancelled" as a suggestion
      and no-where is it used, so we keep it as a string option
*)
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

(* NOTE in the following we parameterise over command/event and args/body types
   because these are auto-generated from schema json *)
(* NOTE we also put a unit on the end of all make functions sigs because
   the opt versions require ?fields and we believe conformity in the api is useful *)

module Request : sig
  type ('cmd, 'args) t
  val seq : ('cmd, 'args) t -> int
  val incr : ('cmd, 'args) t -> ('cmd, 'args) t
  val message : ('cmd, 'args) t -> ProtocolMessage_type.t
  val command : ('cmd, 'args) t -> 'cmd
  val arguments : ('cmd, 'args) t -> 'args
  val enc : 'cmd Data_encoding.t -> 'args Data_encoding.t -> ('cmd, 'args) t Data_encoding.t
  val make : seq:int -> command:'cmd -> arguments:'args -> unit -> ('cmd, 'args) t
end = struct
  type ('cmd, 'args) t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    command : 'cmd;
    arguments : 'args;
  }

  let seq t = t.seq
  let incr t = {t with seq=succ @@ seq t}
  let message t = t.type_
  let command t = t.command
  let arguments t = t.arguments

  let enc cmd args =
    let open Data_encoding in
    conv
      (fun {seq; type_; command; arguments} -> (seq, type_, command, arguments))
      (fun (seq, type_, command, arguments) -> {seq; type_; command; arguments})
      (obj4
         (req "seq" int31)
         (req "type" ProtocolMessage_type.enc)
         (req "command" cmd)
         (req "arguments" args))

  let make ~seq ~command ~arguments () =
    let type_ = ProtocolMessage_type.Request in
    {seq; type_; command; arguments}
end

module MakeRequest (Cmd:ENC0) : sig
  type 'args t
  val seq : 'args t -> int
  val incr : 'args t -> 'args t
  val type_ : 'args t -> ProtocolMessage_type.t
  val command : 'args t -> Cmd.t
  val arguments : 'args t -> 'args
  val enc : 'args Data_encoding.t -> 'args t Data_encoding.t
  val make : seq:int -> arguments:'args -> unit -> 'args t
end = struct

  type 'args t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    command : Cmd.t;
    arguments : 'args;
  }

  let seq t = t.seq
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
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
         (req "command" Cmd.enc)
         (req "arguments" args))

  let make ~seq ~arguments () =
    let type_ = ProtocolMessage_type.Request in
    let command = Cmd.value in
    {seq; type_; command; arguments}

end


module MakeRequest_optionalArgs (Cmd:ENC0) : sig
  type 'args t
  val seq : 'args t -> int
  val incr : 'args t -> 'args t
  val type_ : 'args t -> ProtocolMessage_type.t
  val command : 'args t -> Cmd.t
  val arguments : 'args t -> 'args option
  val enc : 'args Data_encoding.t -> 'args t Data_encoding.t
  val make : seq:int -> ?arguments:'args -> unit -> 'args t
end = struct

  type 'args t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    command : Cmd.t;
    arguments : 'args option;
  }

  let seq t = t.seq
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
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
         (req "command" Cmd.enc)
         (opt "arguments" args))

  let make ~seq ?arguments () =
    let type_ = ProtocolMessage_type.Request in
    let command = Cmd.value in
    {seq; type_; command; arguments}

end



module Response : sig
  type ('cmd, 'body) t

  val seq : ('cmd, 'body) t -> int
  val incr : ('cmd, 'body) t -> ('cmd, 'body) t
  val type_ : ('cmd, 'body) t -> ProtocolMessage_type.t
  val request_seq : ('cmd, 'body) t -> int
  val success : ('cmd, 'body) t -> bool
  val command : ('cmd, 'body) t -> 'cmd
  val message : ('cmd, 'body) t -> string option
  val body : ('cmd, 'body) t -> 'body
  val enc : 'cmd Data_encoding.t -> 'body Data_encoding.t -> ('cmd, 'body) t Data_encoding.t
  val make : seq:int -> request_seq:int -> success:bool -> command:'cmd -> ?message:string -> body:'body -> unit -> ('cmd, 'body) t
end  = struct

  type ('cmd, 'body) t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    request_seq : int;
    success : bool;
    command : 'cmd;
    message : string option; (* only used once I think, keep as string for now *)
    body : 'body;
  }

  let seq t = t. seq
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
  let request_seq t = t.request_seq
  let success t = t.success
  let command t = t.command
  let message t = t.message
  let body t = t.body

  let enc cmd body =
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
         (req "command" cmd)
         (opt "message" string)
         (req "body" body))

  let make ~seq ~request_seq ~success ~command ?message ~body () =
    let type_ = ProtocolMessage_type.Response in
    {seq; type_; request_seq; success; command; message; body}

end


module MakeResponse (Cmd:ENC0) : sig
  type 'body t

  val seq : 'body t -> int
  val incr : 'body t -> 'body t
  val type_ : 'body t -> ProtocolMessage_type.t
  val request_seq : 'body t -> int
  val success : 'body t -> bool
  val command : 'body t -> Cmd.t
  val message : 'body t -> string option
  val body : 'body t -> 'body
  val enc : 'body Data_encoding.t -> 'body t Data_encoding.t
  val make : seq:int -> request_seq:int -> success:bool -> ?message:string -> body:'body -> unit -> 'body t
end  = struct

  type 'body t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    request_seq : int;
    success : bool;
    command : Cmd.t;
    message : string option; (* only used once I think, keep as string for now *)
    body : 'body;
  }

  let seq t = t. seq
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
  let request_seq t = t.request_seq
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
         (req "command" Cmd.enc)
         (opt "message" string)
         (req "body" body))

  let make ~seq ~request_seq ~success ?message ~body () =
    let type_ = ProtocolMessage_type.Response in
    let command = Cmd.value in
    {seq; type_; request_seq; success; command; message; body}

end


module MakeResponse_optionalBody (Cmd:ENC0) : sig
  type 'body t
  val seq : 'body t -> int
  val incr : 'body t -> 'body t
  val type_ : 'body t -> ProtocolMessage_type.t
  val request_seq : 'body t -> int
  val success : 'body t -> bool
  val command : 'body t -> Cmd.t
  val message : 'body t -> string option
  val body : 'body t -> 'body option
  val enc : 'body Data_encoding.t -> 'body t Data_encoding.t
  val make : seq:int -> request_seq:int -> success:bool -> ?message:string -> ?body:'body -> unit -> 'body t
end  = struct

  type 'body t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    request_seq : int;
    success : bool;
    command : Cmd.t;
    message : string option; (* only used once I think, keep as string for now *)
    body : 'body option;
  }

  let seq t = t. seq
  let incr t = {t with seq=succ @@ seq t}
  let type_ t = t.type_
  let request_seq t = t.request_seq
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
         (req "command" Cmd.enc)
         (opt "message" string)
         (opt "body" body))

  let make ~seq ~request_seq ~success ?message ?body () =
    let type_ = ProtocolMessage_type.Response in
    let command = Cmd.value in
    {seq; type_; request_seq; success; command; message; body}

end

module MakeEvent (Ev:ENC0) : sig
  type 'body t
  val seq : 'body t -> int
  val incr : 'body t -> 'body t
  val type_ : 'body t -> ProtocolMessage_type.t
  val event : 'body t -> Ev.t
  val body : 'body t -> 'body
  val enc : 'body Data_encoding.t -> 'body t Data_encoding.t
  val make : seq:int -> body:'body -> unit -> 'body t
end = struct

  type 'body t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    event : Ev.t;
    body : 'body;
  }

  let seq t = t.seq
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
         (req "event" Ev.enc)
         (req "body" body))

  let make ~seq ~body () =
    let type_ = ProtocolMessage_type.Event in
    let event = Ev.value in
    {seq; type_; event; body}

end


module MakeEvent_optionalBody (Ev:ENC0) : sig
  type 'body t
  val seq : 'body t -> int
  val incr : 'body t -> 'body t
  val type_ : 'body t -> ProtocolMessage_type.t
  val event : 'body t -> Ev.t
  val body : 'body t -> 'body option
  val enc : 'body Data_encoding.t -> 'body t Data_encoding.t
  val make : seq:int -> ?body:'body -> unit -> 'body t
end = struct

  type 'body t = {
    seq : int;
    type_ : ProtocolMessage_type.t;
    event : Ev.t;
    body : 'body option;
  }

  let seq t = t.seq
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
         (req "event" Ev.enc)
         (opt "body" body))

  let make ~seq ?body () =
    let type_ = ProtocolMessage_type.Event in
    let event = Ev.value in
    {seq; type_; event; body}

end

