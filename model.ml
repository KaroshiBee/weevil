open Protocol
open Alpha_context

module Weevil_json = struct

  type t = {
    location: int64;
    gas: string;
    stack: string list;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {location; gas; stack} -> (location, gas, stack))
      (fun (location, gas, stack) -> {location; gas; stack})
      (obj3
         (req "location" int64)
         (req "gas" string)
         (req "stack" @@ list string)
      )



end

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

  let to_weevil_json t =
    let (loc_str, gas_str, expr_str) =
      get_location t, get_gas t, get_expr_str t
    in
    let location = Int64.of_string loc_str in
    let gas = gas_str in
    let stack = String.split_on_char ',' expr_str in
    Weevil_json.{location; gas; stack}


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

  (* let add_message t message =
   *   let messages = message :: t.messages in
   *   {t with messages} *)

end

let state = Lwd.var (State.make 0 [])
