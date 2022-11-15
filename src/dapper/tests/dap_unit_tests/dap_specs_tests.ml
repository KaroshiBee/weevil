open Dap_specs
open Alcotest

module MakeModuleNameTests = struct
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0/anyOf/1/oneOf/2/not/properties/thing"

  let test_module_name () =
    let actual = make_module_name path in
    check string "correct module name" "ErrorResponse_thing" actual

end

module ObjectSpecTests = struct
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0"
  let x = Obj_spec.of_path ~dirty_name:"ErrorResponse" ~path ()

  let test_pprint () =
    let actual = Printf.sprintf "%s" @@ Obj_spec.show x in
    let expected = "{ Dap_specs.Obj_spec.safe_name = \"ErrorResponse\";\n  dirty_name = \"ErrorResponse\"; path = /definitions/ErrorResponse/allOf/0;\n  fields = []; is_cyclic = false }" in
    check string "pprint obj spec" expected actual

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
    let y = Enum_spec.set_enums x ~dirty_names:["type"; "module"; "lazy"; "variable"; "null"; "multi Part"] in
    let actual =
      y.enums
      |> List.map (fun (nm:Enum_spec.enum_val) -> Printf.sprintf "%s:%s" nm.safe_name nm.dirty_name)
      |> String.concat ","
    in
    let expected = "Type_:type,Module_:module,Lazy_:lazy,Variable_:variable,Empty_:null,Multi_Part:multi Part" in
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
