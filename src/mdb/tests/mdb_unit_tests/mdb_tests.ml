open Protocol
open Environment
open Alpha_context
open Environment.Error_monad

module TI = Mdb.Mdb_traced_interpreter
module Tbl = Mdb.Mdb_log_records

module TestCfg = struct

  type t =
    | TestLog of int

  let make_log _ctxt _loc _stack _stack_ty =
    TestLog 1

  let unparsing_mode = Script_ir_translator.Readable

  let unparse_stack = function
    | TestLog _i -> Lwt_result_syntax.return []

  let get_loc _ = 100

  (* cant make Gas.t directly *)
  let get_gas _ = Data_encoding.Json.(destruct Gas.encoding (`String "unaccounted"))

end

module TestInterp = TI.T (TestCfg)

(* The tests *)
let test_get_execution_trace_updates () =
  let tbl = Tbl.make () in
  Tbl.add_new tbl 3 @@ TestCfg.TestLog 3;
  Tbl.add_old tbl 2 @@ TestCfg.TestLog 2;
  Tbl.add_new tbl 1 @@ TestCfg.TestLog 1;

  (* check before *)
  let is_in = Tbl.mem tbl in
  let all_in = is_in 1 && is_in 2 && is_in 3 in
  let () =
    Alcotest.(check bool) "contains locs 1,2,3 before" all_in true in

  let logger = TestInterp.trace_logger ~log_records:tbl ~in_channel:stdin () in
  let res = Lwt_main.run @@ TestInterp.get_execution_trace_updates logger in
  match res with
  | Error _ -> failwith "shouldnt be errored"
  | Ok [(100, _, _); (100, _, _)] ->
    (* check after - remember the 100 is the fake loc in the TestCfg *)
    let only_1_3_in = is_in 1 && is_in 3 in
    let () =
      Alcotest.(check bool) "contains locs 1,3 after" only_1_3_in true in

    let two_not_in = is_in 2 in
    Alcotest.(check bool) "doesnt contain loc 2 after" two_not_in false
  | Ok other -> let n = List.length other in failwith @@ Printf.sprintf "should only have loc 100 @ keys 1 and 3, got %d-list" n

let () =
  let open Alcotest in
  run "Traced interpreter" [
      "execution trace", [
          test_case "Only new ones" `Quick test_get_execution_trace_updates;
        ];
    ]
