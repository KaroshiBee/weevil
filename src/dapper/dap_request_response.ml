open Dap_message_exi

module type Request_Response_Link = sig

  type cmd
  type args
  type pargs
  type body
  type pbody

  type t
  val make : ((cmd, args, pargs) RequestMessage.t -> (cmd, body, pbody) ResponseMessage.t result) -> t
  val handle : t -> (request -> response result)
end



module WithSeqr (L:Request_Response_Link)
  : (Request_Response_Link
       with
         type t = L.t and
       type cmd := L.cmd and
       type args := L.args and
       type pargs := L.pargs and
       type body := L.body and
       type pbody := L.pbody)
= struct

  type t = L.t

  let make f =
    let f' = fun req ->
      let request_seq = RequestMessage.seq req in
      let seq = 1 + request_seq in
      f req |> Result.map (fun resp ->
          let resp = ResponseMessage.set_request_seq resp ~request_seq in
          let resp = ResponseMessage.set_seq resp ~seq in
          resp
        )
    in
    L.make f'

  let handle t req =
    L.handle t req

end
