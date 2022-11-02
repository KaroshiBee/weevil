open Dap_message_exi

module type Response_Event_Link = sig
  type t
  type cmd
  type body
  type pbody
  type ev
  type body_
  type pbody_

  val make : ((cmd, body, pbody) ResponseMessage.t -> (ev, body_, pbody_) EventMessage.t result) -> t
  val handle : t -> (response -> event result)
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

  let make f =
    let f' = fun resp ->
      let request_seq = ResponseMessage.seq resp in
      let seq = 1 + request_seq in
      f resp |> Result.map (fun ev ->
          let ev = EventMessage.set_seq ev ~seq in
          ev
        )
    in
    L.make f'

  let handle t resp =
    L.handle t resp

end
