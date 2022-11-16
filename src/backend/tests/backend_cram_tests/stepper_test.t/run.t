testing the backend with an example.tz michelson script, needs to step 8 times
I send in 10 newlines to check that its ok to send them when it is finished

  $ echo "\n \n \n \n \n \n \n \n \n \n" | weevil stepper multiply_2_x_250_equals_500.tz
  
  # log_interp @ location 7
  
  # log_entry @ location 7
  # got ''
  # log_exit @ location 7, line 1
  { "location": 1, "gas": "92.555 units remaining", "stack": [ "" ] }
  
  # log_entry @ location 8
  # got ' '
  # log_exit @ location 8, line 2
  weevil: [INFO] 250
  { "location": 2, "gas": "92.545 units remaining", "stack": [ "250" ] }
  
  # log_entry @ location 11
  # got ' '
  # log_exit @ location 11, line 3
  weevil: [INFO] 2
  weevil: [INFO] 250
  { "location": 3, "gas": "92.535 units remaining", "stack": [ "2", " 250" ] }
  
  # log_entry @ location 14
  # got ' '
  # log_exit @ location 14, line 4
  weevil: [INFO] 500
  { "location": 4, "gas": "92.535 units remaining", "stack": [ "500" ] }
  
  # log_entry @ location 15
  # got ' '
  # log_exit @ location 15, line 5
  { "location": 5, "gas": "92.525 units remaining", "stack": [ "" ] }
  
  # log_entry @ location 16
  # got ' '
  # log_exit @ location 16, line 6
  weevil: [INFO] Unit
  { "location": 6, "gas": "92.515 units remaining", "stack": [ "Unit" ] }
  
  # log_entry @ location 17
  # got ' '
  # log_exit @ location 17, line 7
  weevil: [INFO] {}
  weevil: [INFO] Unit
  { "location": 7, "gas": "92.505 units remaining",  "stack": [ "{}", " Unit" ] }
  
  # log_entry @ location 19
  # got ' '
  # log_exit @ location 19, line 8
  weevil: [INFO] (Pair {} Unit)
  { "location": 8, "gas": "92.495 units remaining",  "stack": [ "(Pair {} Unit)" ] }
  
  # log_entry @ location 6
  # got ' '
  # log_control
