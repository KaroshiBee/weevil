testing with no filename and not headless
  $ weevil_mdb_015 stepper
  weevil_mdb_015: Stepper error - Error:
                                    Invalid_argument("required argument FILE is missing")
                  
                  
  Usage: weevil_mdb_015 stepper [OPTION]… [FILE]
  Try 'weevil_mdb_015 stepper --help' or 'weevil_mdb_015 --help' for more information.
  [124]

testing with no filename in headless mode
  $ weevil_mdb_015 stepper --headless
  Content-Length: 133
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg": "Invalid_argument(\"required argument FILE is missing\")" } ] }
  weevil_mdb_015: 
  [124]

testing with bad filename in headless mode
  $ weevil_mdb_015 stepper -h notthere.tz
  Content-Length: 161
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"notthere.tz\"))" } ] }
  weevil_mdb_015: 
  [124]
