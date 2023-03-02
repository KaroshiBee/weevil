
testing with bad michelson in headless mode
  $ for n in 015 014 013; do echo "weevil_mdb_$n" && weevil_mdb_$n stepper -h bad_michelson.tz; done
  weevil_mdb_015
  Content-Length: 573
  
  { "error":    [ { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",        "location":          { "start": { "line": 2, "column": 7, "point": 39, "byte": 39 },            "stop": { "line": 2, "column": 8, "point": 40, "byte": 40 } },        "token": { "punctuation": "{" } },      { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",        "location":          { "start": { "line": 1, "column": 0, "point": 0, "byte": 0 },            "stop": { "line": 1, "column": 1, "point": 1, "byte": 1 } },        "token": { "punctuation": "{" } } ] }
  weevil_mdb_015: 
  weevil_mdb_014
  Content-Length: 573
  
  { "error":    [ { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",        "location":          { "start": { "line": 2, "column": 7, "point": 39, "byte": 39 },            "stop": { "line": 2, "column": 8, "point": 40, "byte": 40 } },        "token": { "punctuation": "{" } },      { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",        "location":          { "start": { "line": 1, "column": 0, "point": 0, "byte": 0 },            "stop": { "line": 1, "column": 1, "point": 1, "byte": 1 } },        "token": { "punctuation": "{" } } ] }
  weevil_mdb_014: 
  weevil_mdb_013
  Content-Length: 573
  
  { "error":    [ { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",        "location":          { "start": { "line": 2, "column": 7, "point": 39, "byte": 39 },            "stop": { "line": 2, "column": 8, "point": 40, "byte": 40 } },        "token": { "punctuation": "{" } },      { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",        "location":          { "start": { "line": 1, "column": 0, "point": 0, "byte": 0 },            "stop": { "line": 1, "column": 1, "point": 1, "byte": 1 } },        "token": { "punctuation": "{" } } ] }
  weevil_mdb_013: 
  [124]



