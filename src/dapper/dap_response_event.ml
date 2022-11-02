open Dap_message_exi

module type Response_Event_Link = sig
  type t
  type cmd
  type body
  type pbody
  type ev
  type body_
  type pbody_

  val make : ((cmd, body, pbody) ResponseMessage.t -> (ev, body_, pbody_) EventMessage.t Dap_result.t) -> t
  val handle : t -> (response -> event Dap_result.t)
end

module WithSeqr (L:Response_Event_Link)
  : (Response_Event_Link
     with
       type t = L.t and
     type cmd := L.cmd and
     type body := L.body and
     type pbody := L.pbody and
     type ev := L.ev and
     type body_ := L.body_ and
     type pbody_ := L.pbody_)
= struct

  type t = L.t

  let make = L.make

  let handle t resp =
    let request_seq = Dap_utils.ResponseUtils.get_seq resp in
    let seq = 1 + request_seq in
    let setter_ev = Dap_utils.EventUtils.set_sequencing ~seq in
    (* raw message for the error part *)
    let setter_resp_msg msg =
      let msg = ResponseMessage.set_request_seq msg ~request_seq in
      let msg = ResponseMessage.set_seq msg ~seq in
      msg
    in
    L.handle t resp |> Result.map setter_ev |> Result.map_error setter_resp_msg

end
