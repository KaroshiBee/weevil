(* (\* for testing *\) *)
(* module T : functor (S:Types.State_intf) -> Types.String_handler_intf with type state = S.t *)

(* (\* Launching and attaching *\) *)

(* (\* After the debug adapter has been initialized, it is ready to accept requests for starting debugging. Two requests exist for this: *\) *)

(* (\*     launch request: the debug adapter launches the program (“debuggee”) in debug mode and then starts to communicate with it. Since the debug adapter is responsible for launching the debuggee, it should provide a mechanism for the end user to configure the debuggee. For example, passing arguments or specifying a working directory. *\) *)
(* (\*         Debug adapters are free to launch the debuggee however they choose. Typically the debuggee is launched as a child process and its output channels are connected to a client’s debug console via output events. However, this has certain limitations, such as not being able to write to the terminal device directly and not being able to accept standard input. For those cases, launching the debuggee in a terminal is preferable. A debug adapter can use the the runInTerminal request to ask the client to launch the debuggee in a terminal that is integrated into the client or in a terminal that runs outside of the client (but still configured and managed from the client). *\) *)
(* (\*     attach request: the debug adapter connects to an already running program. Here the end user is responsible for launching and terminating the program. *\) *)

(* (\* Since arguments for both requests are highly dependent on a specific debugger and debug adapter implementation, the Debug Adapter Protocol does not specify any arguments for these requests. Instead, the development tool is expected to get information about debugger specific arguments from elsewhere (e.g. contributed by some plugin or extension mechanism) and to build a UI and validation mechanism on top of that. *\) *)
(* (\*   *\\) *\) *)
(* include Types.String_handler_intf *)
