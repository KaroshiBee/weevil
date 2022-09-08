open Protocol
open Alpha_context

module Weevil_json = struct

  type t = {
    location: int;
    gas: string;
    stack: string list;
  }

  let relative_loc t =
    let i = t.location + 3 (* guff at beginning of tz file *) in
    if i > 21 then i+2 else i

  let enc =
    let open Data_encoding in
    conv
      (fun {location; gas; stack} -> (location, gas, stack))
      (fun (location, gas, stack) -> {location; gas; stack})
      (obj3
         (req "location" int31)
         (req "gas" string)
         (req "stack" @@ list string)
      )



end

module Weevil_record = struct

  type t = {
    script_location: Script.location;
    gas: Gas.t;
    expressions: (Script.expr * string option * bool) list;
  }

  let make script_location gas expressions : t =
    {
      script_location;
      gas;
      expressions;
    }


  let get_gas t =
    Format.asprintf "%a" Gas.pp t.gas

  let get_location t =
    Format.sprintf "%d" t.script_location

  let _expr_to_string expr is_ticket =
    let s = Format.asprintf
      "%a"
      Michelson_v1_printer.print_expr
      expr
    in
    Logs.info (fun m -> m "%s" s);
    if is_ticket then
      let spaces = Str.regexp {| +|} in
      let tkt = Str.regexp {|(Pair "tz1.+"|} in
      let r = Str.regexp {|(Pair "tz1.+" "\(.+\)")|} in
      Str.replace_first tkt {|(Ticket "tz1.."|} s
      |> Str.replace_first r {|\1|}
      |> Defaults._replace "\n" ""
      |> Str.global_replace spaces " "
    else
      s

  let get_expr_str ?(sep=", ") t =
    t.expressions
    |> List.map (fun (expr, _, is_ticket) -> _expr_to_string expr is_ticket)
    |> String.concat sep

  let get_annotations ?(sep=", ")t =
    t.expressions
    |> List.map (fun (_expr, s_opt, _) -> Option.value ~default:"NOT ANNOTATED" s_opt)
    |> String.concat sep

  let to_weevil_json t =
    let (loc_str, gas_str, expr_str) =
      get_location t, get_gas t, get_expr_str t
    in
    let location = int_of_string loc_str in
    let gas = gas_str in
    let stack = String.split_on_char ',' expr_str in
    Weevil_json.{location; gas; stack}


end

module Weevil_trace = struct

  type t = Weevil_record.t array

  let of_execution_trace (execution_trace : Script_typed_ir.execution_trace) : t =
    execution_trace
    |> List.map (fun (loc, gas, exprs) ->
        let exprs = (exprs |> List.map (fun (e, a) -> (e, a, false))) in
        Weevil_record.make loc gas exprs
      )
    |> Array.of_list

  let get_safe t i =
    let n = Array.length t in
    if i < n then (Array.get t i) else (Array.get t (n-1))

end


module State = struct

  type t = {
    line_number: int;
    current_seq: int;
    stack_frames: Dapper.Dap_base.StackFrame.t list;
    scopes: Dapper.Dap_base.Scope.t list;
    variables: Dapper.Dap_base.Variable_.t list;
  }

  let make line_number current_seq =
    {
      line_number;
      current_seq;
      stack_frames=[];
      scopes=[];
      variables=[];
    }

  let incr_line_number t max_lines current_seq =
    let line_number = min max_lines (t.line_number + 1) |> max 0 in
    {t with line_number; current_seq}

  let decr_line_number t max_lines =
    let line_number = min max_lines (t.line_number - 1) |> max 0 in
    {t with line_number}

  (* let add_message t message =
   *   let messages = message :: t.messages in
   *   {t with messages} *)

end

let state = Lwd.var (State.make 1 0)
