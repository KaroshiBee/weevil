open Tezos_protocol_014_PtKathma.Protocol
open Alpha_context

(* NOTE type unparsing_mode = Optimized | Readable | Optimized_legacy, could we phantom this into the logger type? *)

type t =
  | Log :
      context
      * Tezos_micheline.Micheline_parser.location
      * ('a * 's)
      * ('a, 's) Script_typed_ir.stack_ty
      -> t

let make_log ctxt loc stack stack_ty =
  Log (ctxt, loc, stack, stack_ty)

let unparsing_mode = Script_ir_translator.Readable

let unparse_stack = function
  | Log (ctxt_in, _loc, stack, stack_ty) ->
    let open Tezos_protocol_014_PtKathma.Environment.Error_monad in
    let open Lwt_result_syntax in
    (* We drop the gas limit as this function is only used for debugging/errors. *)
    let ctxt = Gas.set_unlimited ctxt_in in
    let rec _unparse_stack :
      type a s.
      (a, s) Script_typed_ir.stack_ty * (a * s) ->
      (Script.expr * string option * bool) list tzresult Lwt.t = function
      | Bot_t, (EmptyCell, EmptyCell) -> return_nil
      | Item_t (ty, rest_ty), (v, rest) ->
        let* (data, _ctxt) = Script_ir_translator.unparse_data ctxt unparsing_mode ty v in
        let+ rest = _unparse_stack (rest_ty, rest) in
        (* TODO do we need to strip locations? *)
        let data = Tezos_protocol_014_PtKathma.Environment.Micheline.strip_locations data in
        (data, None, false) :: rest
    in
    _unparse_stack (stack_ty, stack)

let get_loc = function
  | Log (_, loc, _, _) -> loc

let get_gas = function
  | Log (ctxt, _, _, _) -> Gas.level ctxt
