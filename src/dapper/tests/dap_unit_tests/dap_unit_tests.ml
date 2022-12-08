open Alcotest

let () =
  run "Dapper" [
    "Dap_header - content length", [
      test_case "extracts correct number - simple" `Quick Dap_header_tests.HeaderTests.test_content_length_simple;
      test_case "extracts correct number - complex" `Quick Dap_header_tests.HeaderTests.test_content_length_complex;
    ];

    "Dap_specs - path wrangling", [
      test_case "module names" `Quick Dap_specs_tests.MakeModuleNameTests.test_module_name;
      test_case "object path" `Quick Dap_specs_tests.ObjectSpecTests.test_path;
      test_case "enum path" `Quick Dap_specs_tests.EnumSpecTests.test_path;
    ];

    "Dap_specs - object properties", [
      test_case "object is big" `Quick Dap_specs_tests.ObjectSpecTests.test_is_big;
      test_case "show object" `Quick Dap_specs_tests.ObjectSpecTests.test_pprint;
    ];

    "Dap_specs - enum properties", [
      test_case "show enum" `Quick Dap_specs_tests.EnumSpecTests.test_pprint;
      test_case "enum set path names" `Quick Dap_specs_tests.EnumSpecTests.test_set_names;
      test_case "enum append path name" `Quick Dap_specs_tests.EnumSpecTests.test_append_name;
      test_case "enum append path names" `Quick Dap_specs_tests.EnumSpecTests.test_append_names;
    ];

    "Dap_specs - spec properties", [
      test_case "to string" `Quick Dap_specs_tests.SpecsTests.test_to_string;
      test_case "dirty name" `Quick Dap_specs_tests.SpecsTests.test_dirty_name;
      test_case "is special" `Quick Dap_specs_tests.SpecsTests.test_is_special;
    ];

    "Dap_request - equality checks", Dap_request_tests.RequestTests.suite_basic_equality_checks;
    "Dap_request - js destruct/construct the same", Dap_request_tests.RequestTests.suite_js_roundtrip;

    "Dap_response - equality checks", Dap_response_tests.ResponseTests.suite_basic_equality_checks;
    "Dap_response - js destruct/construct the same", Dap_response_tests.ResponseTests.suite_js_roundtrip;

    "Dap_event - equality checks", Dap_event_tests.EventTests.suite_basic_equality_checks;
    "Dap_event - js destruct/construct the same", Dap_event_tests.EventTests.suite_js_roundtrip;
  ]
