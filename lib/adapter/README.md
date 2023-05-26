# Request/Response sequence

A simple overview of the request/response sequences that are supported in the ```weevil```.

The IDE (emacs/vscode/vim etc) will be sending the ```requests```; the ```weevil``` will return with ```responses``` and may also raise ```events```.

Please refer to the [specification](https://microsoft.github.io/debug-adapter-protocol/specification) for full details.

Also the ```handlers``` list in [handler](./handler.ml) for the entrypoints in the ```weevil``` code.

## 1 - starting a session ```initialize``` 

Initialises the ```weevil``` with information such as:

- the name of the IDE,
- the format of file paths, native or uri,
- whether line and column values are 0 or 1 based,
- the locale used by the development tool. A debug adapter is expected to return error messages that honor this locale.

The ```weevil``` responds with the capabilities it supports.

## 2 - ```launch``` or ```attach```

These messages are used to tell the ```weevil``` to start the debugging program (referred to as the debuggee).  

Depending on the mode specified it should either launch the debuggee or attach to an already running instance of the debuggee.  

## 3 - breakpoint management TODO

## 4 - ```configurationDone```

When the IDE is ready to start stepping through code it sends the ```configurationDone``` request to signal that all configuration of breakpoints etc has finished.

## 5 - stepping 

Currently the ```weevil``` only supports single-stepping one ```Michelson``` instruction at a time.

### next

This message is used to tell the debugger to make one step forward, currently only the granularity of one ```Michelson``` instruction is supported.

### continue/stepIn/stepOut/pause TODO 

## 6 - program state

When the debugger stops the ```weevil``` sends the ```stopped``` event message to the IDE. 

At these points the IDE can query the ```weevil``` for its current debuggee state.

This follows a standard hierarchy of ```threads```, ```stack```, ```scopes```, ```variables```...```variables```.

### threads

A request to retrieve all currently active threads.

### stack

A list of stack frames for a specified thread.

### scopes

The scopes (container of variables) for a given stack frame.

### variables

The variables for a given scope.  Variables can be structured/composite so multiple ```variables``` requests can be involved when a user wants to drill down through a composite variable structure.

## 7 - restarting TODO

## 8 - ending a session

Ending a session has slightly different sequences of responses depending on how the ```weevil``` debuggee was started.

### terminate

The ```weevil``` supports the ```terminate``` capability and so the IDE can use this request to cleanly shutdown the ```weevil``` and debuggee program (if it was launched by the ```weevil```).

If that process fails to shutdown, and it is in ```launch``` mode,  then a subsequent ```terminate``` request will then use the ```disconnect``` request to forcefully shutdown the ```weevil``` (see below).

### disconnect

For debuggees that have been attached to, the ```disconnect``` request detaches the debugger from debuggee but allows the debuggee program to continue.

For debuggees that have been launched by the ```weevil```, the ```disconnect``` request is used to forcefully end the debug session c.f. the ```terminate``` request for the clean shutdown sequence.

### terminate event 

If the ```weevil``` wants to end a debug session then it raises the ```terminated``` event.
