open Dap_message_exi

module type Raise_Event_Link = sig

  type ev
  type body
  type pbody
  type ev_
  type body_
  type pbody_

  type t
  val make : ((ev, body, pbody) EventMessage.t -> (ev_, body_, pbody_) EventMessage.t Dap_result.t) -> t
  val handle : t -> (event -> event Dap_result.t)
end



module WithSeqr (L:Raise_Event_Link)
  : (Raise_Event_Link
       with
         type t = L.t and
       type ev := L.ev and
       type body := L.body and
       type pbody := L.pbody and
       type ev_ := L.ev_ and
       type body_ := L.body_ and
       type pbody_ := L.pbody_)
= struct

  type t = L.t

  let make = L.make

  let handle t ev =
    let request_seq = Dap_utils.EventUtils.get_seq ev in
    let seq = 1 + request_seq in
    let setter_ev = Dap_utils.EventUtils.set_sequencing ~seq in
    let setter_resp_msg msg =
      let msg = ResponseMessage.set_request_seq msg ~request_seq in
      let msg = ResponseMessage.set_seq msg ~seq in
      msg
    in
    L.handle t ev |> Result.map setter_ev |> Result.map_error setter_resp_msg

end
