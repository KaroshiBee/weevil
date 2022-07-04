open Protocol
open Alpha_context

module Weevil_record = struct

  type t = {
    script_location: Script.location;
    gas: Gas.t;
    expressions: (Script.expr * string option) list;
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

  let _expr_to_string expr =
    Format.asprintf
      "%a"
      Michelson_v1_printer.print_expr
      expr

  let get_expr_str ?(sep=", ") t =
    t.expressions
    |> List.map (fun (expr, _) -> _expr_to_string expr)
    |> String.concat sep

  let get_annotations ?(sep=", ")t =
    t.expressions
    |> List.map (fun (_expr, s_opt) -> Option.value ~default:"NOT ANNOTATED" s_opt)
    |> String.concat sep


end

module Weevil_trace = struct

  type t = Weevil_record.t array

  let of_execution_trace (execution_trace : Script_typed_ir.execution_trace) : t =
    execution_trace
    |> List.map (fun (loc, gas, exprs) -> Weevil_record.make loc gas exprs)
    |> Array.of_list

  let get_safe t i =
    let n = Array.length t in
    if i < n then (Array.get t i) else (Array.get t (n-1))

end


module State = struct

  type t = {
    line_number: int;
    messages: string list;
  }

  let make line_number messages =
    {
      line_number;
      messages;
    }

  let incr_line_number t max_lines =
    let line_number = min max_lines (t.line_number + 1) |> max 0 in
    {t with line_number}

  let decr_line_number t max_lines =
    let line_number = min max_lines (t.line_number - 1) |> max 0 in
    {t with line_number}

  let add_message t message =
    let messages = message :: t.messages in
    {t with messages}

end

let state = Lwd.var (State.make 0 [])
