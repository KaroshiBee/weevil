open Dap_base

module Request = struct

  type t =
    | Cancel
    | Next

  let enc_t =
    let open Data_encoding in
    conv
      (function | Cancel -> "cancel" | Next -> "next")
      (function | "cancel" -> Cancel | "next" -> Next | _ -> failwith "Unknown request")
      string

  type 'json cls_t = <
    ProtocolMessage.cls_t;
    command:t;
    arguments:'json
  >

  class ['json] cls
      (seq:int64)
      (command:t)
      (arguments:'json)
      = object
    inherit ProtocolMessage.cls seq Request

    method command = command
    method arguments = arguments

  end

  let enc js =
    let open Data_encoding in
    conv
      (fun (r : < 'json cls_t >) ->
         (r#seq, r#type_, r#command, r#arguments) )

      (fun (seq, _, command, arguments) ->
         new cls seq command arguments)

      (obj4
         (req "seq" int64)
         (req "type" ProtocolMessage.enc_t)
         (req "command" enc_t)
         (req "arguments" js)
      )

end

module CancelArguments = struct

  type t = {
    requestId:int64 option;
    progressId:string option
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {requestId; progressId} -> (requestId, progressId))
      (fun (requestId, progressId) -> {requestId; progressId})
      (obj2
         (opt "requestId" int64)
         (opt "progressId" string))

end


module CancelRequest = struct

  type args = CancelArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int64) (arguments:args) = object
    inherit [args] Request.cls seq Cancel arguments
  end

  let enc = Request.enc CancelArguments.enc

end


module NextArguments = struct

  type t = {
    threadId: int64;
    singleThread: bool option;
    granularity: SteppingGranularity.t option;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {threadId; singleThread; granularity} -> (threadId, singleThread, granularity))
      (fun (threadId, singleThread, granularity) -> {threadId; singleThread; granularity})
      (obj3
         (req "threadId" int64)
         (opt "singleThread" bool)
         (opt "granularity" SteppingGranularity.enc)
      )

end

module NextRequest = struct

  type args = NextArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int64) (arguments:args) = object
    inherit [args] Request.cls seq Next arguments
  end

  let enc = Request.enc NextArguments.enc

end

(* class ['json] requester_cls (req:'json Request.cls_t) = object
 *   method req = req
 * end *)
