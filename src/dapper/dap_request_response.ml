open Dap_message_exi

module type Request_response_link = sig
  type cmd

  type args

  type pargs

  type body

  type pbody

  type t

  (* NOTE the cmd param is the same for both *)
  val make :
    ((cmd, args, pargs) RequestMessage.t ->
    (cmd, body, pbody) ResponseMessage.t Dap_result.t) ->
    t

  val handle : t -> request -> response Dap_result.t
end

module WithSeqr (L : Request_response_link) :
  Request_response_link
    with type t = L.t
     and type cmd := L.cmd
     and type args := L.args
     and type pargs := L.pargs
     and type body := L.body
     and type pbody := L.pbody = struct
  type t = L.t

  let make = L.make

  let handle t req =
    let request_seq = Dap_utils.RequestUtils.get_seq req in
    let seq = 1 + request_seq in
    let setter_resp = Dap_utils.ResponseUtils.set_sequencing ~seq ~request_seq in
    (* the error part is a raw message *)
    let setter_resp_msg msg =
      let msg = ResponseMessage.set_seq msg ~seq in
      let msg = ResponseMessage.set_request_seq msg ~request_seq in
      msg
    in
    L.handle t req |> Dap_result.map setter_resp |> Dap_result.map_error setter_resp_msg
end
