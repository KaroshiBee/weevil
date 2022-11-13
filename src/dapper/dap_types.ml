module Err = Dap_response

module type STATE_T = sig
  type t

  val make : unit -> t

  val current_seqr : t -> Dap_base.Seqr.t

  val set_seqr : t -> Dap_base.Seqr.t -> unit
end

(* the parts of the Request/Response?Event_messages that we need *)
module type MSG_T = sig
  type ('enum, 'contents, 'presence) t

  val seq : (_, _, _) t -> int

  val set_seq :
    seq:Dap_base.Seqr.t ->
    ('enum, 'contents, 'presence) t ->
    ('enum, 'contents, 'presence) t
end

(* the parts of the Request/Response/Event GADTs that we need *)
module type GADT_T = sig
  type 'a t

  type 'a expr

  val fmap_ : ('a -> 'b) -> ('a -> 'b) t

  val val_ : 'msg t -> 'msg expr

  val map_ : ('msg -> 'b) expr * 'msg expr -> 'b expr

  val eval : 'msg. 'msg expr -> 'msg
end

(* the parts of the combined ('a,'b,'c) Msg.t Thing.t that we need,
   parameterised by msg/gadt modules *)
module FULL_T (Msg : MSG_T) (Obj : GADT_T) = struct
  module type T = sig
    type enum

    type contents

    type presence

    type msg = (enum, contents, presence) Msg.t

    type t = msg Obj.t

    val ctor : msg -> t

    val enc : msg Data_encoding.t
  end
end

(* output sig for linking an input to an output e.g. request -> response *)
module type LINK_T = sig
  type in_t

  type out_t

  type state

  type t

  val make : handler:(state -> in_t -> out_t Dap_result.t) -> t

  val handle :
    t ->
    state:state ->
    string ->
    (string, string) Lwt_result.t
end
