run the backend server and connect

  $ weevil backend 9001 &
  weevil: [INFO] [MICH] starting backend server on port 9001
  weevil: [INFO] [MICH] got connection
  weevil: [INFO] [MICH] got msg '{ "event": "dune exec -- weevil stepper multiply_2_x_25_equals_50.tz" }'
  weevil: [INFO] [MICH] starting new stepper with cmd 'dune exec -- weevil stepper multiply_2_x_25_equals_50.tz'
  weevil: [INFO] [MICH] stepper_process_start
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [MICH] got msg '{ "event": 1 }'
  weevil: [INFO] [MICH] Got Next request
  { "event": 1 }
  
  weevil: [INFO] [MICH] got msg '{ "event": 1 }'
  weevil: [INFO] [MICH] Got Next request
  { "event": 1 }
  
  weevil: [INFO] [MICH] got msg '{ "event": 1 }'
  weevil: [INFO] [MICH] Got Next request
  { "event": 1 }
  
  weevil: [INFO] [STEPPER] got msg from subprocess ''
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '# log_interp @ location 7'
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess ''
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '# log_entry @ location 7'
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '# got 'step''
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '# log_exit @ location 7, line 1'
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '{ "location": 1, "gas": "92.555 units remaining", "stack": [ "" ] }'
  weevil: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 1, "gas": "92.555 units remaining", "stack": [ "" ] }'
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess ''
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '# log_entry @ location 8'
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '# got 'step''
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '# log_exit @ location 8, line 2'
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '{ "location": 2, "gas": "92.545 units remaining", "stack": [ "25" ] }'
  weevil: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 2, "gas": "92.545 units remaining", "stack": [ "25" ] }'
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess ''
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '# log_entry @ location 11'
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '# got 'step''
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '# log_exit @ location 11, line 3'
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '{ "location": 3, "gas": "92.535 units remaining", "stack": [ "2", " 25" ] }'
  weevil: [INFO] [STEPPER] got weevil log record from subprocess '{ "location": 3, "gas": "92.535 units remaining", "stack": [ "2", " 25" ] }'
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess ''
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [STEPPER] got msg from subprocess '# log_entry @ location 14'
  weevil: [INFO] [STEPPER] step handler start, waiting for messages
  weevil: [INFO] [MICH] got msg '{ "event": {} }'
  weevil: [INFO] [MICH] connection closed
  weevil: [INFO] [STEPPER] subprocess complete

connect to weevil, need the sleep to establish the process
  $ sleep 1 && test_runner backend multiply_2_x_25_equals_50.tz

tidy up
  $ kill $(pgrep weevil)

check is cleaned up
  $ ps -a | grep weevil #should be blank
  [1]
  $ netstat -lp | grep 9001
  (Not all processes could be identified, non-owned process info
   will not be shown, you would have to be root to see it all.)
  [1]
