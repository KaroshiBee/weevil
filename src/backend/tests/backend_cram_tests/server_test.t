run the backend server and connect

  $ weevil backend 9001 &
  weevil: [INFO] [MICH] starting backend server on port 9001
  weevil: [INFO] [MICH] got connection
  weevil: [INFO] [MICH] connection closed

telnet to connect to weevil, need the sleep to establish the process
  $ sleep 1 && telnet localhost 9001
  Trying 127.0.0.1...
  Connected to localhost.
  Escape character is '^]'.
  Connection closed by foreign host.
  [1]

tidy up
  $ kill $(pgrep weevil)

check is cleaned up
  $ ps -a | grep weevil #should be blank
  [1]
  $ netstat -lp | grep 9001
  (Not all processes could be identified, non-owned process info
   will not be shown, you would have to be root to see it all.)
  [1]
