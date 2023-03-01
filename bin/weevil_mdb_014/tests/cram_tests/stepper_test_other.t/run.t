testing with no filename and not headless
  $ weevil_mdb_014 stepper
  weevil_mdb_014: Stepper error - Error:
                                    Invalid_argument("required argument FILE is missing")
                  
                  
  Usage: weevil_mdb_014 stepper [OPTION]… [FILE]
  Try 'weevil_mdb_014 stepper --help' or 'weevil_mdb_014 --help' for more information.
  [124]

testing with no filename in headless mode
  $ weevil_mdb_014 stepper --headless
  Content-Length: 133
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg": "Invalid_argument(\"required argument FILE is missing\")" } ] }
  weevil_mdb_014: 
  [124]

testing with bad filename in headless mode
  $ weevil_mdb_014 stepper -h notthere.tz
  Content-Length: 161
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"notthere.tz\"))" } ] }
  weevil_mdb_014: 
  [124]
