open Dap_message_exi


type init_ev = ( Dap_events.initialized, EmptyObject.t option, Presence.opt ) EventMessage.t
type stopped_ev = (Dap_events.stopped, StoppedEvent_body.t, Presence.req) EventMessage.t
type cont_ev = (Dap_events.continued, ContinuedEvent_body.t, Presence.req) EventMessage.t
type break_ev = (Dap_events.breakpoint, BreakpointEvent_body.t, Presence.req) EventMessage.t

type _ value =
  | InitializedEv : init_ev -> init_ev value
  | StoppedEv : stopped_ev -> stopped_ev value
  | ContinuedEv : cont_ev -> cont_ev value
  | BreakpointEv : break_ev -> break_ev value
  | Fmap : ('a -> 'b) -> ('a -> 'b) value

type _ expr =
  | Val : 'msg value -> 'msg expr
  | Map : ('msg -> 'b) expr * 'msg expr -> 'b expr

let rec eval : type msg. msg expr -> msg = function
  | Val (InitializedEv ev) -> ev
  | Val (StoppedEv ev) -> ev
  | Val (ContinuedEv ev) -> ev
  | Val (BreakpointEv ev) -> ev
  | Val (Fmap f) -> f
  | Map (f, v) -> let f' = (eval f) and v' = eval v in (f' v')

let set_seq seq = Fmap (fun evt -> EventMessage.set_seq evt ~seq)
let get_seq = Fmap (fun evt -> EventMessage.seq evt)

let i = InitializedEv (
    Dap_utils.default_event_opt Dap_events.initialized (EmptyObject.make ())
  )

let c = ContinuedEv (
    Dap_utils.default_event_req Dap_events.continued (ContinuedEvent_body.make ~threadId:0 ())
  )

let x = eval @@ Map (Val (set_seq 1), Val i)
let y = eval @@ Map (Val (set_seq 2), Val c)

let xx = eval @@ Map (Val get_seq, Val i)
let yy = eval @@ Map (Val get_seq, Val (ContinuedEv y))

let wrapper f = fun v ->
  let seq = succ @@ eval @@ Map (Val get_seq, Val v) in
  let v' = f v in
  eval @@ Map (Val (set_seq seq), Val v')

let f1 = wrapper @@ fun _ -> i
let f2 = wrapper @@ fun _ -> c

let link : ('ev, _, _) EventMessage.t value -> ('ev, _, _) EventMessage.t value -> int = fun e1 e2 ->
  let i = eval @@ Map (Val get_seq, Val e1) in
  let j = eval @@ Map (Val get_seq, Val e2) in
  i+j

let _ = link i @@ InitializedEv x

module type Raise_event_link = sig

  type ev
  type body
  type pbody
  type ev_
  type body_
  type pbody_

  type t
  val lift : (ev, body, pbody) EventMessage.t -> (ev, body, pbody) EventMessage.t value
  val make : ((ev, body, pbody) EventMessage.t -> (ev_, body_, pbody_) EventMessage.t Dap_result.t) -> t
  val handle : t -> (event -> event Dap_result.t)
end



module WithSeqr (L:Raise_event_link)
  : (Raise_event_link
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

  let lift = L.lift

  let make = L.make

  let handle t ev =
    let request_seq = Dap_utils.EventUtils.get_seq ev in
    let seq = 1 + request_seq in
    let setter_ev = Dap_utils.EventUtils.set_sequencing ~seq in
    (* raw message for the error part *)
    let setter_resp_msg msg =
      let msg = ResponseMessage.set_request_seq msg ~request_seq in
      let msg = ResponseMessage.set_seq msg ~seq in
      msg
    in
    L.handle t ev |> Dap_result.map setter_ev |> Dap_result.map_error setter_resp_msg

end



(* let f = Fmap (fun evt -> *)
(*       let seq = 1 + EventMessage.seq evt in *)
(*       let evt' = Dap_utils.default_event_req Dap_events.continued (ContinuedEvent_body.make ~threadId:0 ()) in *)
(*       EventMessage.set_seq evt' ~seq *)
(*         ) *)

(* let x = eval @@ Map (Val f, Val i) *)
(* let y = eval @@ Map (Val f, Val c) *)

(* let app f v = eval @@ Map (Val (Fmap f), Val v) *)
