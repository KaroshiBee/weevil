testing with no filename and not headless
  $ weevil_mdb_015 stepper 2>&1 | sed 's/weevil_mdb_015\://' | sed 's/weevil_mdb_015\://' | sed 's/\r\n*//g' | sed 's/^ *//g' | sed 's/ *$//g'
  Stepper error - Error:
  Invalid_argument("required argument FILE is missing")
  
  Usage: weevil_mdb_015 stepper [OPTION]… [FILE]
  Try 'weevil_mdb_015 stepper --help' or 'weevil_mdb_015 --help' for more information.

testing with no filename in headless mode
  $ weevil_mdb_015 stepper --headless 2>&1 | sed 's/weevil_mdb_015\://' | sed 's/\r\n*//g' | sed 's/^ *//g' | sed 's/ *$//g'
  
  Content-Length: 133
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg": "Invalid_argument(\"required argument FILE is missing\")" } ] }

testing with bad filename in headless mode
  $ weevil_mdb_015 stepper -h notthere.tz 2>&1 | sed 's/weevil_mdb_015\://' | sed 's/\r\n*//g' | sed 's/^ *//g' | sed 's/ *$//g'
  
  Content-Length: 161
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"notthere.tz\"))" } ] }
