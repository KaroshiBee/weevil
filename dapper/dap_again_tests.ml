open Dap_again

let%expect_test "Check Req" =
  let path = [`Field "definitions"; `Field "CancelRequest"] in
  let x = Req.of_path_exn ~path ~args:"CancelArgs" () in

  Printf.printf "%s" (Q.json_pointer_of_path x.path);
  [%expect {| /definitions/CancelRequest |}];

  Printf.printf "%s" x.command;
  [%expect {| Cancel |}];

  Printf.printf "%s" (x.args |> Option.get);
  [%expect {| CancelArgs |}];

  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0" in
  try
    let _ = Req.of_path_exn ~path () in ()
  with Req.Not_request err ->
    Printf.printf "%s" err;
  [%expect {| /definitions/ErrorResponse/allOf/0 |}]



let%expect_test "Check Resp" =
  let path = [`Field "definitions"; `Field "CancelResponse"] in
  let x = Resp.of_path_exn ~path ~body:"CancelArgs" ~message:"Message" () in

  Printf.printf "%s" (Q.json_pointer_of_path x.path);
  [%expect {| /definitions/CancelResponse |}];

  Printf.printf "%s" x.command;
  [%expect {| Cancel |}];

  Printf.printf "%s" (x.body |> Option.get);
  [%expect {| CancelArgs |}];

  Printf.printf "%s" (x.message |> Option.get);
  [%expect {| Message |}];

  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0" in
  try
    let _ = Resp.of_path_exn ~path () in ()
  with Resp.Not_response err ->
    Printf.printf "%s" err;
  [%expect {| /definitions/ErrorResponse/allOf/0 |}]


let%expect_test "Check Event" =
  let path = [`Field "definitions"; `Field "CancelEvent"] in
  let x = Event.of_path_exn ~path ~body:"CancelArgs" () in

  Printf.printf "%s" (Q.json_pointer_of_path x.path);
  [%expect {| /definitions/CancelEvent |}];

  Printf.printf "%s" x.event;
  [%expect {| Cancel |}];

  Printf.printf "%s" (x.body |> Option.get);
  [%expect {| CancelArgs |}];

  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0" in
  try
    let _ = Event.of_path_exn ~path () in ()
  with Event.Not_event err ->
    Printf.printf "%s" err;
  [%expect {| /definitions/ErrorResponse/allOf/0 |}]


let%expect_test "Check obj" =
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0" in
  let x = Obj.of_path ~path () in

  Printf.printf "%s" (Q.json_pointer_of_path x.path);
  [%expect {| /definitions/ErrorResponse/allOf/0 |}];

  Printf.printf "%s" (Q.json_pointer_of_path (Obj.root_path x));
  [%expect {| /definitions/ErrorResponse |}];

  Printf.printf "%b" (Obj.is_big x);
  [%expect {| false |}]


let%expect_test "Check enum" =
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0" in
  let x = Enum.of_path ~path () in

  Printf.printf "%s" (Q.json_pointer_of_path x.path);
  [%expect {| /definitions/ErrorResponse/allOf/0 |}];

  let x = Enum.set_enums x ~dirty_names:["type"; "module"; "lazy"; "null"; "multi Part"] in
  x.enums
  |> List.map (fun Enum.{safe_name; dirty_name} -> Printf.sprintf "%s:%s" safe_name dirty_name)
  |> String.concat ","
  |> Printf.printf "%s";
  [%expect {| Type_:type,Module_:module,Lazy_:lazy,Empty_:null,Multi_Part:multi Part |}];

  let x = Enum.append_enum x ~dirty_name:"field" in
  x.enums
  |> List.map (fun Enum.{safe_name; dirty_name} -> Printf.sprintf "%s:%s" safe_name dirty_name)
  |> String.concat ","
  |> Printf.printf "%s";
  [%expect {| Field:field,Type_:type,Module_:module,Lazy_:lazy,Empty_:null,Multi_Part:multi Part |}]
