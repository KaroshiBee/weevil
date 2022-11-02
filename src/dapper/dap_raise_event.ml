open Dap_message_exi

module type Raise_Event_Link = sig

  type ev
  type body
  type pbody
  type ev_
  type body_
  type pbody_

  type t
  val make : ((ev, body, pbody) EventMessage.t -> (ev_, body_, pbody_) EventMessage.t result) -> t
  val handle : t -> (event -> event result)
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

  let make f =
    let f' = fun req ->
      let request_seq = EventMessage.seq req in
      let seq = 1 + request_seq in
      f req |> Result.map (fun resp ->
          let resp = EventMessage.set_seq resp ~seq in
          resp
        )
    in
    L.make f'

  let handle t ev =
    L.handle t ev

end
