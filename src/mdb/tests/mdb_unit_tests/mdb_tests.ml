open Protocol
open Environment
open Alpha_context
open Environment.Error_monad

module TI = Mdb.Mdb_traced_interpreter_t
module Tbl = Mdb.Mdb_log_records

module TestCfg = struct

  module P = Protocol
  module Ctxt = P.Alpha_context
  module Env = Environment
  module Err = Environment.Error_monad

  type t =
    | TestLog of int

  let make_log _ctxt _loc _stack _stack_ty =
    TestLog 1

  let unparsing_mode = Script_ir_translator.Readable

  let unparse_stack = function
    | TestLog _i -> Lwt_result_syntax.return []

  let get_loc _ = Tezos_micheline.Micheline_parser.{
      start={point=0; byte=0; line=100; column=2};
      stop={point=10; byte=0; line=110; column=20};
    }

  (* cant make Gas.t directly *)
  let get_gas _ = Data_encoding.Json.(destruct Gas.encoding (`String "unaccounted"))

end

module TestInterp = TI.T (TestCfg)

(* The tests *)
let test_get_execution_trace_updates () =
  let tbl = Tbl.make () in
  Tbl.add_new tbl @@ TestCfg.TestLog 3;
  Tbl.add_old tbl @@ TestCfg.TestLog 2;
  Tbl.add_new tbl @@ TestCfg.TestLog 1;

  (* check before *)
  let is_in = Tbl.mem tbl in
  let all_in = is_in 0 && is_in 1 && is_in 2 in
  let () =
    Alcotest.(check bool) "contains indices 0,1,2 before" all_in true
  in

  let () =
    match Tbl.to_list tbl with
    | [TestCfg.TestLog 3; TestCfg.TestLog 1;] -> ()
    | xs ->
        xs |> List.iter (function TestCfg.TestLog x -> Printf.printf "x:%d\n" x);
        failwith "should return sorted list by index of only new ones"
  in

  let () = Tbl.new_to_old_inplace ~keep_old:false tbl in
  let _2_not_in = not @@ is_in 1 in
  let () =
    Alcotest.(check bool) "no longer contains locs 2 (index 1) after" _2_not_in true
  in
  let _1_3_in = is_in 0 && is_in 2 in
  let () =
    Alcotest.(check bool) "still contains locs 1,3 after (ix: 0,2)" _1_3_in true
  in

  let () =
    match Tbl.to_list tbl with
    | [] -> ()
    | _ -> failwith "should not have any new entries left"
  in
  let () = Tbl.remove_all tbl in
  let nothing_in = not (is_in 0 || is_in 1 || is_in 2) in
  Alcotest.(check bool) "contains nothing after remove all" nothing_in true


let () =
  let open Alcotest in
  run "Traced interpreter" [
      "log records table", [
          test_case "Only new ones" `Quick test_get_execution_trace_updates;
        ];
    ]
