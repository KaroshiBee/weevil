open Dap_specs
open Alcotest

module ObjectSpecTests = struct
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0"
  let x = Obj_spec.of_path ~dirty_name:"ErrorResponse" ~path ()

  let test_path () =
    let actual = Q.json_pointer_of_path x.path in
    check string "correct path" "/definitions/ErrorResponse/allOf/0" actual

  let test_is_big () =
    let actual = Obj_spec.is_big x in
    check bool "is big" false actual
end

module EnumSpecTests = struct
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0"
  let x = Enum_spec.of_path ~dirty_name:"ErrorResponse" ~path ()

  let test_path () =
    let actual = Q.json_pointer_of_path x.path in
    check string "correct path" "/definitions/ErrorResponse/allOf/0" actual

  let test_set_names () =
    let y = Enum_spec.set_enums x ~dirty_names:["type"; "module"; "lazy"; "null"; "multi Part"] in
    let actual =
      y.enums
      |> List.map (fun (nm:Enum_spec.enum_val) -> Printf.sprintf "%s:%s" nm.safe_name nm.dirty_name)
      |> String.concat ","
    in
    let expected = "Type_:type,Module_:module,Lazy_:lazy,Empty_:null,Multi_Part:multi Part" in
    check string "correct enum names" expected actual

  let test_append_name () =
    let z = Enum_spec.append_enum x ~dirty_name:"field" in
    let actual =
      z.enums
      |> List.map (fun (nm:Enum_spec.enum_val) -> Printf.sprintf "%s:%s" nm.safe_name nm.dirty_name)
      |> String.concat ","
    in
    let expected = "Field:field" in
    check string "correct enum names" expected actual

end

let () =
  run "Dap_specs" [
    "path wrangling", [
      test_case "object path" `Quick ObjectSpecTests.test_path;
      test_case "enum path" `Quick EnumSpecTests.test_path;
      test_case "enum set path names" `Quick EnumSpecTests.test_set_names;
      test_case "enum append path names" `Quick EnumSpecTests.test_append_name;
    ];

    "object properties", [
      test_case "object is big" `Quick ObjectSpecTests.test_is_big;
    ]
  ]
