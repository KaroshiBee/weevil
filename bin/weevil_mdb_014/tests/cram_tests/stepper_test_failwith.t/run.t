testing a contract that has a failwith, it should incrementally step up to it and then fail
  $ echo "\n \n \n \n \n \n \n \n \n \n" | weevil_mdb_014 stepper --headless --storage '0' --parameter '(Pair 7 5)' failwith.tz
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
  [124]

