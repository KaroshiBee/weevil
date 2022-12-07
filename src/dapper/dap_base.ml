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
module Gen = struct

  let gen_utf8_str =
    QCheck.Gen.string_printable
  let gen_utf8_str_opt =
    QCheck.Gen.option gen_utf8_str
  let gen_utf8_str_list =
    QCheck.Gen.list gen_utf8_str
  let gen_utf8_str_list_opt =
    QCheck.Gen.(option @@ list gen_utf8_str)

  let gen_int31 =
    let mn, mx = Int32.(min_int |> to_int, max_int |> to_int) in
    let mn, mx = 1 + mn/2, -1 + mx/2 in
    fun st -> QCheck.Gen.int_range mn mx st
  let gen_int31_opt =
    QCheck.Gen.option gen_int31
  let gen_int31_list_opt =
    QCheck.Gen.(option @@ list gen_int31)

  (* TODO proper gen for json *)
  let gen_json = QCheck.Gen.(map (fun x -> `String x) gen_utf8_str)
  let gen_json_opt = QCheck.Gen.option gen_json

  let convert_ocaml_type_str ~as_annot = function
    | "string" -> Some (if as_annot then "[@gen Gen.gen_utf8_str]" else "Gen.gen_utf8_str")
    | "string option" -> Some (if as_annot then "[@gen Gen.gen_utf8_str_opt]" else "Gen.gen_utf8_str_opt")
    | "string list" -> Some (if as_annot then "[@gen Gen.gen_utf8_str_list]" else "Gen.gen_utf8_str_list")
    | "string list option" -> Some (if as_annot then "[@gen Gen.gen_utf8_str_list_opt]" else "Gen.gen_utf8_str_list_opt")
    | "int" -> Some (if as_annot then "[@gen Gen.gen_int31]" else "Gen.gen_int31")
    | "int option" -> Some (if as_annot then "[@gen Gen.gen_int31_opt]" else "Gen.gen_int31_opt")
    | "int list option" -> Some (if as_annot then "[@gen Gen.gen_int31_list_opt]" else "Gen.gen_int31_list_opt")
    | "Data_encoding.json" -> Some (if as_annot then "[@gen Gen.gen_json]" else "Gen.gen_json")
    | "Data_encoding.json option" -> Some (if as_annot then "[@gen Gen.gen_json_opt]" else "Gen.gen_json_opt")
    | _ -> None

end
module Eq = struct
  let equal_json = fun x y -> Data_encoding.Json.(String.equal (to_string x) (to_string y))
  let equal_json_opt = Option.equal equal_json

  let convert_ocaml_type_str ~as_annot = function
    | "Data_encoding.json" -> Some (if as_annot then "[@equal Eq.equal_json]" else "Eq.equal_json")
    | "Data_encoding.json option" -> Some (if as_annot then "[@equal Eq.equal_json_opt]" else "Eq.equal_json_opt")
    | _ -> None
end


(* some helper modules *)
module EmptyObject : sig
  type t
  val equal : t -> t -> bool
  val module_name : string
  val enc : t Data_encoding.t
  val gen : t QCheck.Gen.t
  val arb : t QCheck.arbitrary
  val make : unit -> t
end = struct

  let module_name = "EmptyObject"

  type t = unit [@@deriving qcheck, eq]

  let enc = Data_encoding.empty

  let make () = ()

end

module IntString : sig
  type t
  val equal : t -> t -> bool
  val module_name : string
  val enc : t Data_encoding.t
  val gen : t QCheck.Gen.t
  val arb : t QCheck.arbitrary
  val of_int : int -> t
  val of_string : string -> t
end = struct

  let module_name = "IntString"

  type t =
    | I of int [@gen Gen.gen_int31]
    | S of string [@gen Gen.gen_utf8_str]
  [@@deriving qcheck, eq]


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

(* launching and attaching requests are special - weevil defines what should go into them *)
module LaunchRequestArguments : sig
  type t
  val equal : t -> t -> bool
  val module_name : string
  val enc : t Data_encoding.t
  val gen : t QCheck.Gen.t
  val arb : t QCheck.arbitrary
  val make : ?restart:Data_encoding.json -> ?noDebug:bool -> script_filename:string -> storage:string -> parameter:string -> unit -> t
  val restart : t -> Data_encoding.json option
  val noDebug : t -> bool option
  val script_filename : t -> string
  val storage : t -> string
  val parameter : t -> string
end = struct

  let module_name = "LaunchRequestArguments"
  type t = {
    restart: (Data_encoding.json option  [@gen Gen.gen_json_opt][@equal Eq.equal_json_opt]);
    noDebug: bool option;
    (* Since launching is debugger/runtime specific, the arguments for this request are not part of this specification.
       Arguments for launch request. Additional attributes are implementation specific.
    *)
    script_filename: (string [@gen Gen.gen_utf8_str]);
    storage: (string [@gen Gen.gen_utf8_str]);
    parameter: (string [@gen Gen.gen_utf8_str]);
  }
  [@@deriving qcheck, eq]

  let enc =
    let open Data_encoding in
    conv
      (fun {restart; noDebug; script_filename; storage; parameter; } -> (restart, noDebug, script_filename, storage, parameter))
      (fun (restart, noDebug, script_filename, storage, parameter) -> {restart; noDebug; script_filename; storage; parameter;})
      (obj5
         (opt "__restart" json)
         (opt "noDebug" bool)
         (req "script_filename" string)
         (req "storage" string)
         (req "parameter" string)
      )

  let make ?restart ?noDebug ~script_filename ~storage ~parameter () =
    {restart; noDebug; script_filename; storage; parameter;}

  let restart t = t.restart
  let noDebug t = t.noDebug
  let script_filename t = t.script_filename
  let storage t = t.storage
  let parameter t = t.parameter

end

module AttachRequestArguments : sig
  type t
  val equal : t -> t -> bool
  val module_name : string
  val enc : t Data_encoding.t
  val gen : t QCheck.Gen.t
  val arb : t QCheck.arbitrary
  val make : ?restart:Data_encoding.json -> script_filename:string -> storage:string -> parameter:string -> unit -> t
  val restart : t -> Data_encoding.json option
  val script_filename : t -> string
  val storage : t -> string
  val parameter : t -> string
end = struct

  let module_name = "AttachRequestArguments"
  type t = {
    restart: (Data_encoding.json option  [@gen Gen.gen_json_opt][@equal Eq.equal_json_opt]);
    (* Since attaching is debugger/runtime specific, the arguments for this request are not part of this specification.
       Arguments for attach request. Additional attributes are implementation specific.
    *)
    script_filename: (string [@gen Gen.gen_utf8_str]);
    storage: (string [@gen Gen.gen_utf8_str]);
    parameter: (string [@gen Gen.gen_utf8_str]);
  }
  [@@deriving qcheck, eq]

  let enc =
    let open Data_encoding in
    conv
      (fun {restart; script_filename; storage; parameter; } -> (restart, script_filename, storage, parameter))
      (fun (restart, script_filename, storage, parameter) -> {restart; script_filename; storage; parameter;})
      (obj4
         (opt "__restart" json)
         (req "script_filename" string)
         (req "storage" string)
         (req "parameter" string)
      )

  let make ?restart ~script_filename ~storage ~parameter () =
    {restart; script_filename; storage; parameter;}

  let restart t = t.restart
  let script_filename t = t.script_filename
  let storage t = t.storage
  let parameter t = t.parameter

end

module RestartArguments : sig
  type t
  val equal : t -> t -> bool
  val module_name : string
  val enc : t Data_encoding.t
  val gen : t QCheck.Gen.t
  val arb : t QCheck.arbitrary
  val of_launch_args : LaunchRequestArguments.t -> t
  val of_attach_args : AttachRequestArguments.t -> t
end = struct

  let module_name = "RestartArguments"

  type t =
    | LaunchRequestArgs of LaunchRequestArguments.t
    | AttachRequestArgs of AttachRequestArguments.t
  [@@deriving qcheck, eq]

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
  [@@deriving qcheck, eq]

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

module Presence = struct
  type req
  type opt
end

module type SEQR = sig
  type t
  val make : seq:int -> ?request_seq:int -> unit -> t
  val seq : t -> int
  val request_seq : t -> int
  val not_set : int
end


module Seqr : SEQR = struct
  type t = {
    seq: int;
    request_seq: int;
  }

  (* NOTE for use in the HANDLERs so seq and request_seq are calculated for us *)
  let not_set = -1

  let make ~seq ?request_seq () =
    let request_seq = Option.value ~default:(-1) request_seq in
    {seq; request_seq}

  let seq t = t.seq
  let request_seq t = t.request_seq

end
