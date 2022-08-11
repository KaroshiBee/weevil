open Notty
open Nottui
module W = Nottui_widgets
open Lwd_infix

module DRq = Dapper.Dap_request
module DRs = Dapper.Dap_response
module DEv = Dapper.Dap_event
module Db = Dapper.Dap_base
module Js = Data_encoding.Json


module Btn_stackFrames = struct

  type t = {
    old_state: Model.State.t;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
  }

  let make old_state ic oc =
    {old_state; ic; oc}

  let render {old_state; ic; oc} oc_log =
    let btn_text = Printf.sprintf "[+]" in
    let btn_action () =
      let next_seq = Int64.succ old_state.current_seq in
      let args = DRq.StackTraceArguments.{threadId=Defaults._THE_THREAD_ID; startFrame=None; levels=None} in
      let req = new DRq.StackTraceRequest.cls next_seq args in
      let js = Js.construct DRq.StackTraceRequest.enc req |> Defaults.wrap_header in
      ignore (
        Lwt_io.write_line oc js >>= fun _ ->
        Lwt_io.read_line ic >>= fun _hdr ->
        Lwt_io.write_line oc_log _hdr >>= fun _ ->
        Lwt_io.read_line ic >>= fun _break ->
        Lwt_io.write_line oc_log "break" >>= fun _ ->
        Lwt_io.read_line ic >>= fun _resp ->
        Lwt_io.write_line oc_log _resp >>= fun _ ->
        (* TODO extract seq number and update state if success=true *)
        let current_seq = Int64.add next_seq 1L in
        Lwd.set Model.state Model.State.{old_state with current_seq} |> Lwt.return
      )
    in
    [
      Ui.hcat [
        W.printf ~attr:A.empty " Stack frames ";
        W.button ~attr:A.(bg lightblue) btn_text btn_action;
      ]
    ]


end


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
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
  }

  let make old_state lines trace ic oc =
    let max_lines = List.length lines in
    {old_state; max_lines; trace; ic; oc}

  let render {old_state; max_lines; trace; ic; oc} oc_log =
    assert (0 = Array.length trace);
    (* let ln = min max_lines old_state.line_number |> max 0 in
     * let gas = "TODO gas" in (\* Model.Weevil_trace.get_safe trace ln |> Model.Weevil_record.get_gas in *\)
     * let loc = "TODO loc" in (\* Model.Weevil_trace.get_safe trace ln |> Model.Weevil_record.get_location in *\)
     * let exprs = "TODO expr" in (\* Model.Weevil_trace.get_safe trace ln |> Model.Weevil_record.get_expr_str in *\) *)

    (* button actions need to unpause the stepper thread *)
    let btn_text_down = Printf.sprintf "[-]" in
    let btn_action_down () =
      Lwd.set Model.state (Model.State.decr_line_number old_state max_lines)
    in
    let btn_text_up = Printf.sprintf "[+]" in
    let btn_action_up () =
      let next_seq = Int64.succ old_state.current_seq in
      let args = DRq.NextArguments.{threadId=Defaults._THE_THREAD_ID; singleThread=None; granularity=None} in
      let req = new DRq.NextRequest.cls next_seq args in
      let js = Js.construct DRq.NextRequest.enc req |> Defaults.wrap_header in
      ignore (
        Lwt_io.write_line oc js >>= fun _ ->
        Lwt_io.read_line ic >>= fun _hdr ->
        Lwt_io.write_line oc_log _hdr >>= fun _ ->
        Lwt_io.read_line ic >>= fun _break ->
        Lwt_io.write_line oc_log "break" >>= fun _ ->
        Lwt_io.read_line ic >>= fun _resp ->
        Lwt_io.write_line oc_log _resp >>= fun _ ->
        Lwt_io.read_line ic >>= fun _hdr ->
        Lwt_io.write_line oc_log _hdr >>= fun _ ->
        Lwt_io.read_line ic >>= fun _break ->
        Lwt_io.write_line oc_log "break" >>= fun _ ->
        Lwt_io.read_line ic >>= fun _stopped_ev ->
        Lwt_io.write_line oc_log _stopped_ev >>= fun _ ->
        (* TODO extract seq number and update state if success=true *)
        let current_seq = Int64.add next_seq 2L in
        Lwd.set Model.state (Model.State.incr_line_number old_state max_lines current_seq) |> Lwt.return
      )
    in
    [
      Ui.hcat [
        W.button ~attr:A.(bg lightred) btn_text_down btn_action_down;
        W.button ~attr:A.(bg lightgreen) btn_text_up btn_action_up;
        W.printf ~attr:A.empty "                                      ";
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

(* TODO
   pass contract code from command line and on to stepper

*)
let ui_main (ic:Lwt_io.input_channel) (oc:Lwt_io.output_channel) (oc_log:Lwt_io.output_channel) =
  let$ st = Lwd.get Model.state in
  let lines = split_out_code Defaults._THE_CONTRACT in
  let trace = [||] in
  let instr = W.printf "Mouse CLICK [-] or [+] to go back or forward resp.\nPress ALT-q to quit\n\n" in
  let step_btn = Btn_next.make st lines trace ic oc in
  let src_code_panel = Src_code.make st.line_number lines in
  let stackframe_btn = Btn_stackFrames.make st ic oc in
  let code = [Btn_next.render step_btn oc_log; Src_code.render src_code_panel;]|> List.concat |> Ui.vcat in
  let state = Btn_stackFrames.render stackframe_btn oc_log |> Ui.hcat in
  let main = Ui.hcat [code; state] in
  Ui.vcat [instr; main]
