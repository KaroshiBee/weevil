testing a contract that has a failwith, it should incrementally step up to it and then fail
  $ for n in 015 014 013 012 011; do echo "weevil_mdb_$n" && echo "\n \n \n \n \n \n \n \n \n \n" | weevil_mdb_$n stepper --headless --storage '0' --parameter '(Pair 7 5)' failwith.tz; done
  weevil_mdb_015
  Content-Length: 220
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.879 units remaining", "stack": [ "(Pair (Pair 7 5) 0)" ] }
  Content-Length: 211
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.869 units remaining", "stack": [ "(Pair 7 5)" ] }
  Content-Length: 225
  
  { "location":    { "start": { "line": 4, "column": 7, "point": 61, "byte": 61 },      "stop": { "line": 4, "column": 10, "point": 64, "byte": 64 } },  "gas": "91.859 units remaining", "stack": [ "(Pair 7 5)", "(Pair 7 5)" ] }
  Content-Length: 221
  
  { "location":    { "start": { "line": 5, "column": 7, "point": 74, "byte": 74 },      "stop": { "line": 5, "column": 13, "point": 80, "byte": 80 } },  "gas": "91.849 units remaining", "stack": [ "7", "5", "(Pair 7 5)" ] }
  Content-Length: 216
  
  { "location":    { "start": { "line": 6, "column": 7, "point": 90, "byte": 90 },      "stop": { "line": 6, "column": 14, "point": 97, "byte": 97 } },  "gas": "91.814 units remaining", "stack": [ "1", "(Pair 7 5)" ] }
  Content-Length: 222
  
  { "location":    { "start": { "line": 7, "column": 7, "point": 107, "byte": 107 },      "stop": { "line": 7, "column": 9, "point": 109, "byte": 109 } },  "gas": "91.804 units remaining", "stack": [ "True", "(Pair 7 5)" ] }
  Content-Length: 215
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.804 units remaining", "stack": [ "(Pair 7 5)" ] }
  Content-Length: 212
  
  { "location":    { "start": { "line": 8, "column": 13, "point": 125, "byte": 125 },      "stop": { "line": 8, "column": 19, "point": 131, "byte": 131 } },  "gas": "91.794 units remaining", "stack": [ "7", "5" ] }
  Content-Length: 219
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.769 units remaining", "stack": [ "True", "7", "5" ] }
  Content-Length: 230
  
  { "location":    { "start": { "line": 9, "column": 7, "point": 143, "byte": 143 },      "stop": { "line": 9, "column": 24, "point": 160, "byte": 160 } },  "gas": "91.759 units remaining", "stack": [ "\"BOO\"", "True", "7", "5" ] }
  Content-Length: 368
  
  { "error":    [ { "kind": "temporary",        "id": "proto.015-PtLimaPt.michelson_v1.runtime_error",        "contract_handle": "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi",        "contract_code": "Deprecated" },      { "kind": "temporary",        "id": "proto.015-PtLimaPt.michelson_v1.script_rejected",        "location": 20, "with": { "string": "BOO" }, "trace": [] } ] }
  weevil_mdb_015: 
  weevil_mdb_014
  Content-Length: 220
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.834 units remaining", "stack": [ "(Pair (Pair 7 5) 0)" ] }
  Content-Length: 211
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.824 units remaining", "stack": [ "(Pair 7 5)" ] }
  Content-Length: 225
  
  { "location":    { "start": { "line": 4, "column": 7, "point": 61, "byte": 61 },      "stop": { "line": 4, "column": 10, "point": 64, "byte": 64 } },  "gas": "91.814 units remaining", "stack": [ "(Pair 7 5)", "(Pair 7 5)" ] }
  Content-Length: 221
  
  { "location":    { "start": { "line": 5, "column": 7, "point": 74, "byte": 74 },      "stop": { "line": 5, "column": 13, "point": 80, "byte": 80 } },  "gas": "91.804 units remaining", "stack": [ "7", "5", "(Pair 7 5)" ] }
  Content-Length: 216
  
  { "location":    { "start": { "line": 6, "column": 7, "point": 90, "byte": 90 },      "stop": { "line": 6, "column": 14, "point": 97, "byte": 97 } },  "gas": "91.769 units remaining", "stack": [ "1", "(Pair 7 5)" ] }
  Content-Length: 222
  
  { "location":    { "start": { "line": 7, "column": 7, "point": 107, "byte": 107 },      "stop": { "line": 7, "column": 9, "point": 109, "byte": 109 } },  "gas": "91.759 units remaining", "stack": [ "True", "(Pair 7 5)" ] }
  Content-Length: 215
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.759 units remaining", "stack": [ "(Pair 7 5)" ] }
  Content-Length: 212
  
  { "location":    { "start": { "line": 8, "column": 13, "point": 125, "byte": 125 },      "stop": { "line": 8, "column": 19, "point": 131, "byte": 131 } },  "gas": "91.749 units remaining", "stack": [ "7", "5" ] }
  Content-Length: 219
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.729 units remaining", "stack": [ "True", "7", "5" ] }
  Content-Length: 230
  
  { "location":    { "start": { "line": 9, "column": 7, "point": 143, "byte": 143 },      "stop": { "line": 9, "column": 24, "point": 160, "byte": 160 } },  "gas": "91.719 units remaining", "stack": [ "\"BOO\"", "True", "7", "5" ] }
  Content-Length: 368
  
  { "error":    [ { "kind": "temporary",        "id": "proto.014-PtKathma.michelson_v1.runtime_error",        "contract_handle": "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi",        "contract_code": "Deprecated" },      { "kind": "temporary",        "id": "proto.014-PtKathma.michelson_v1.script_rejected",        "location": 20, "with": { "string": "BOO" }, "trace": [] } ] }
  weevil_mdb_014: 
  weevil_mdb_013
  Content-Length: 220
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.309 units remaining", "stack": [ "(Pair (Pair 7 5) 0)" ] }
  Content-Length: 211
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.299 units remaining", "stack": [ "(Pair 7 5)" ] }
  Content-Length: 225
  
  { "location":    { "start": { "line": 4, "column": 7, "point": 61, "byte": 61 },      "stop": { "line": 4, "column": 10, "point": 64, "byte": 64 } },  "gas": "91.289 units remaining", "stack": [ "(Pair 7 5)", "(Pair 7 5)" ] }
  Content-Length: 221
  
  { "location":    { "start": { "line": 5, "column": 7, "point": 74, "byte": 74 },      "stop": { "line": 5, "column": 13, "point": 80, "byte": 80 } },  "gas": "91.279 units remaining", "stack": [ "7", "5", "(Pair 7 5)" ] }
  Content-Length: 216
  
  { "location":    { "start": { "line": 6, "column": 7, "point": 90, "byte": 90 },      "stop": { "line": 6, "column": 14, "point": 97, "byte": 97 } },  "gas": "91.244 units remaining", "stack": [ "1", "(Pair 7 5)" ] }
  Content-Length: 222
  
  { "location":    { "start": { "line": 7, "column": 7, "point": 107, "byte": 107 },      "stop": { "line": 7, "column": 9, "point": 109, "byte": 109 } },  "gas": "91.234 units remaining", "stack": [ "True", "(Pair 7 5)" ] }
  Content-Length: 215
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.224 units remaining", "stack": [ "(Pair 7 5)" ] }
  Content-Length: 212
  
  { "location":    { "start": { "line": 8, "column": 13, "point": 125, "byte": 125 },      "stop": { "line": 8, "column": 19, "point": 131, "byte": 131 } },  "gas": "91.214 units remaining", "stack": [ "7", "5" ] }
  Content-Length: 219
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.194 units remaining", "stack": [ "True", "7", "5" ] }
  Content-Length: 230
  
  { "location":    { "start": { "line": 9, "column": 7, "point": 143, "byte": 143 },      "stop": { "line": 9, "column": 24, "point": 160, "byte": 160 } },  "gas": "91.184 units remaining", "stack": [ "\"BOO\"", "True", "7", "5" ] }
  Content-Length: 368
  
  { "error":    [ { "kind": "temporary",        "id": "proto.013-PtJakart.michelson_v1.runtime_error",        "contract_handle": "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi",        "contract_code": "Deprecated" },      { "kind": "temporary",        "id": "proto.013-PtJakart.michelson_v1.script_rejected",        "location": 20, "with": { "string": "BOO" }, "trace": [] } ] }
  weevil_mdb_013: 
  weevil_mdb_012
  Content-Length: 220
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.184 units remaining", "stack": [ "(Pair (Pair 7 5) 0)" ] }
  Content-Length: 211
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.174 units remaining", "stack": [ "(Pair 7 5)" ] }
  Content-Length: 225
  
  { "location":    { "start": { "line": 4, "column": 7, "point": 61, "byte": 61 },      "stop": { "line": 4, "column": 10, "point": 64, "byte": 64 } },  "gas": "91.164 units remaining", "stack": [ "(Pair 7 5)", "(Pair 7 5)" ] }
  Content-Length: 221
  
  { "location":    { "start": { "line": 5, "column": 7, "point": 74, "byte": 74 },      "stop": { "line": 5, "column": 13, "point": 80, "byte": 80 } },  "gas": "91.154 units remaining", "stack": [ "7", "5", "(Pair 7 5)" ] }
  Content-Length: 216
  
  { "location":    { "start": { "line": 6, "column": 7, "point": 90, "byte": 90 },      "stop": { "line": 6, "column": 14, "point": 97, "byte": 97 } },  "gas": "91.119 units remaining", "stack": [ "1", "(Pair 7 5)" ] }
  Content-Length: 222
  
  { "location":    { "start": { "line": 7, "column": 7, "point": 107, "byte": 107 },      "stop": { "line": 7, "column": 9, "point": 109, "byte": 109 } },  "gas": "91.104 units remaining", "stack": [ "True", "(Pair 7 5)" ] }
  Content-Length: 215
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.089 units remaining", "stack": [ "(Pair 7 5)" ] }
  Content-Length: 212
  
  { "location":    { "start": { "line": 8, "column": 13, "point": 125, "byte": 125 },      "stop": { "line": 8, "column": 19, "point": 131, "byte": 131 } },  "gas": "91.079 units remaining", "stack": [ "7", "5" ] }
  Content-Length: 219
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.049 units remaining", "stack": [ "True", "7", "5" ] }
  Content-Length: 230
  
  { "location":    { "start": { "line": 9, "column": 7, "point": 143, "byte": 143 },      "stop": { "line": 9, "column": 24, "point": 160, "byte": 160 } },  "gas": "91.039 units remaining", "stack": [ "\"BOO\"", "True", "7", "5" ] }
  Content-Length: 368
  
  { "error":    [ { "kind": "temporary",        "id": "proto.012-Psithaca.michelson_v1.runtime_error",        "contract_handle": "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi",        "contract_code": "Deprecated" },      { "kind": "temporary",        "id": "proto.012-Psithaca.michelson_v1.script_rejected",        "location": 20, "with": { "string": "BOO" }, "trace": [] } ] }
  weevil_mdb_012: 
  weevil_mdb_011
  Content-Length: 220
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.184 units remaining", "stack": [ "(Pair (Pair 7 5) 0)" ] }
  Content-Length: 211
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.174 units remaining", "stack": [ "(Pair 7 5)" ] }
  Content-Length: 225
  
  { "location":    { "start": { "line": 4, "column": 7, "point": 61, "byte": 61 },      "stop": { "line": 4, "column": 10, "point": 64, "byte": 64 } },  "gas": "91.164 units remaining", "stack": [ "(Pair 7 5)", "(Pair 7 5)" ] }
  Content-Length: 221
  
  { "location":    { "start": { "line": 5, "column": 7, "point": 74, "byte": 74 },      "stop": { "line": 5, "column": 13, "point": 80, "byte": 80 } },  "gas": "91.154 units remaining", "stack": [ "7", "5", "(Pair 7 5)" ] }
  Content-Length: 216
  
  { "location":    { "start": { "line": 6, "column": 7, "point": 90, "byte": 90 },      "stop": { "line": 6, "column": 14, "point": 97, "byte": 97 } },  "gas": "91.119 units remaining", "stack": [ "1", "(Pair 7 5)" ] }
  Content-Length: 222
  
  { "location":    { "start": { "line": 7, "column": 7, "point": 107, "byte": 107 },      "stop": { "line": 7, "column": 9, "point": 109, "byte": 109 } },  "gas": "91.104 units remaining", "stack": [ "True", "(Pair 7 5)" ] }
  Content-Length: 215
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.089 units remaining", "stack": [ "(Pair 7 5)" ] }
  Content-Length: 212
  
  { "location":    { "start": { "line": 8, "column": 13, "point": 125, "byte": 125 },      "stop": { "line": 8, "column": 19, "point": 131, "byte": 131 } },  "gas": "91.079 units remaining", "stack": [ "7", "5" ] }
  Content-Length: 219
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.049 units remaining", "stack": [ "True", "7", "5" ] }
  Content-Length: 230
  
  { "location":    { "start": { "line": 9, "column": 7, "point": 143, "byte": 143 },      "stop": { "line": 9, "column": 24, "point": 160, "byte": 160 } },  "gas": "91.039 units remaining", "stack": [ "\"BOO\"", "True", "7", "5" ] }
  Content-Length: 1034
  
  { "error":    [ { "kind": "temporary",        "id": "proto.011-PtHangz2.michelson_v1.runtime_error",        "contract_handle": "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi",        "contract_code":          [ { "prim": "parameter",              "args":                [ { "prim": "pair",                    "args": [ { "prim": "int" }, { "prim": "int" } ] } ] },            { "prim": "storage", "args": [ { "prim": "int" } ] },            { "prim": "code",              "args":                [ [ { "prim": "CAR" }, { "prim": "DUP" },                    { "prim": "UNPAIR" }, { "prim": "COMPARE" },                    { "prim": "GT" },                    { "prim": "DIP", "args": [ [ { "prim": "UNPAIR" } ] ] },                    { "prim": "PUSH",                      "args": [ { "prim": "string" }, { "string": "BOO" } ] },                    { "prim": "FAILWITH" } ] ] } ] },      { "kind": "temporary",        "id": "proto.011-PtHangz2.michelson_v1.script_rejected",        "location": 20, "with": { "string": "BOO" }, "trace": [] } ] }
  weevil_mdb_011: 
  [124]

