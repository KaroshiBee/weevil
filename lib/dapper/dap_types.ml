module Err = Dap_response

(* state in the Dapper is only concerned with sequencing *)
module type STATE_T = sig
  type t

  val make : unit -> t

  val current_seqr : t -> Dap_base.Seqr.t

  val set_seqr : t -> Dap_base.Seqr.t -> unit
end

(* the parts of the Request/Response/Event_messages that we need *)
module type MSG_READONLY_T = sig
  type ('enum, 'contents, 'presence) t

  val seq : (_, _, _) t -> int
end

module type MSG_T = sig
  include MSG_READONLY_T

  val set_seq :
    seqr:Dap_base.Seqr.t ->
    ('enum, 'contents, 'presence) t ->
    ('enum, 'contents, 'presence) t
end

(* the parts of the Request/Response/Event GADTs that we need for the handlers *)
module type GADT_T = sig
  type 'a t

  type 'a expr

  val eval : 'a. 'a expr -> 'a

  val map_f : f:('msg -> 'a) -> 'msg t -> 'a expr

  val map2_f : f:('msg1 -> 'msg2 -> 'a) -> 'msg1 t -> 'msg2 t -> 'a expr

  val equal : equal_f:('msg1 -> 'msg2 -> bool) -> 'msg1 t -> 'msg2 t -> bool expr

  (* TODO still not sure we want to be able to extract?
     but we do restrict seq modification to just the inside of the dap_handlers functors *)
  val extract : 'msg t -> 'msg

end

(* the parts of the combined ('a,'b,'c) Msg.t Thing.t that we need,
   to define handlers parameterised by msg/gadt modules,
   we distinguish between ones that can modify seq numbers and ones that cant *)
module FULL_READONLY_T (Msg : MSG_READONLY_T) (Obj : GADT_T) = struct
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

  val make :
    handler:(state:state -> in_t -> out_t Dap_result.t) ->
    (state:state -> string -> (string, string) Lwt_result.t)

end
