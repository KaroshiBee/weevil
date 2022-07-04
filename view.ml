open Notty
open Nottui
module W = Nottui_widgets
open Lwd_infix


module Src_frag = struct

  type t = {
    highlighted: bool;
    src_line: string;
  }

  let make ?(highlighted=false) src_line =
    {highlighted; src_line}

  let render {highlighted; src_line} =
    let attr = if highlighted then A.(bg blue) else A.(bg black) in
    let src = if highlighted then Printf.sprintf "%s" src_line else src_line in
    [W.string ~attr src]

end

module Src_code = struct
  type t = {
    src_lines: Src_frag.t list
  }

  let make line_number lines =
    let src_lines = Src_frag.(
      lines
      |> List.mapi (fun i line ->
          let highlighted = i = line_number in
          make ~highlighted line)
    ) in
    {src_lines}

  let render {src_lines} =
    let ui_lines = List.fold_left (fun acc line ->
        let ui_line = Src_frag.render line in
        ui_line :: acc
      ) [] src_lines in
    ui_lines |> List.concat |> List.rev

end


module Btn_next = struct

  type t = {
    old_state: Model.State.t;
    max_lines:int;
    trace: Model.Weevil_record.t array;
  }

  let make old_state lines trace =
    let max_lines = List.length lines in
    {old_state; max_lines; trace}

  let render {old_state; max_lines; trace} =
    let ln = min max_lines old_state.line_number |> max 0 in
    let gas = Model.Weevil_trace.get_safe trace ln |> Model.Weevil_record.get_gas in
    let loc = Model.Weevil_trace.get_safe trace ln |> Model.Weevil_record.get_location in
    let exprs = Model.Weevil_trace.get_safe trace ln |> Model.Weevil_record.get_expr_str in

    (* button actions need to unpause the stepper thread *)
    let btn_text_down = Printf.sprintf "[-]" in
    let btn_action_down () =
      Lwd.set Model.state (Model.State.decr_line_number old_state max_lines)
    in
    let btn_text_up = Printf.sprintf "[+]" in
    let btn_action_up () =
      Lwd.set Model.state (Model.State.incr_line_number old_state max_lines)
    in
    [
      Ui.hcat [
        W.button ~attr:A.(bg lightred) btn_text_down btn_action_down;
        W.button ~attr:A.(bg lightgreen) btn_text_up btn_action_up;
        W.printf ~attr:A.empty " %d - GAS %s at Location - %s: STACK [ %s ]\n" ln gas loc exprs;
      ]
    ]

end


let split_out_code contract =
  let on = "code {" in
  match contract |> Stringext.cut ~on with
  | None -> []
  | Some (param_storage, src_code) ->
    let src_lines =
      src_code
      |> String.split_on_char ';'
      |> List.map (Stringext.replace_all ~pattern:"}}" ~with_:"")
      |> List.map (fun s -> "  " ^ s)
    in
    List.concat [ [(param_storage ^ on)];
                  src_lines;
                  ["}}"]
                ]

let ui_main contract trace =
  let$ st = Lwd.get Model.state in
  let lines = split_out_code contract in
  let instr = [W.printf "Mouse CLICK [-] or [+] to go back or forward resp.\nPress ALT-q to quit\n\n"] in
  let btn = Btn_next.make st lines trace in
  let src_code_panel = Src_code.make st.line_number lines in
  [instr; Btn_next.render btn; Src_code.render src_code_panel] |> List.concat |> Ui.vcat
