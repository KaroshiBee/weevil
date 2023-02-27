let%expect_test "check deduplicate_stable" =
  let xs = [] in
  let s = Utils.Helpers.deduplicate_stable xs |> String.concat "," in
  Printf.printf "%s" s;
  [%expect {||}];

  let xs = [555;111;222;222;333;111;444] |> List.map string_of_int in
  let s = Utils.Helpers.deduplicate_stable xs |> String.concat "," in
  Printf.printf "%s" s;
  [%expect {| 555,111,222,333,444 |}]
