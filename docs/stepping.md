# Stepping Through Michelson Code

## Introduction 

In this document we describe the approach taken to allow the ```weevil``` debug adapter to step through Michelson code one executable expression at a time.

The code for this implementation is found in the [mdb](../lib/mdb) submodule of the ```weevil``` tool with Tezos protocol-specific executables in the [bin](../bin) directory.

### First pass

One simple way to implement stepping for a given contract and entrypoint would be to run the whole thing from start to finish with the desired input and storage and record a trace of everything that occured.  This trace could then form the basis of a debugging system that allows one to step back and forth at will.

This was in fact the approach taken for the very first prototype of the ```weevil``` tool.  

It is also a mode of execution supported by the [octez-client](https://tezos.gitlab.io/active/cli-commands.html) ```run script``` command with the ```--trace-stack``` option.  In this mode the main Michelson interpreter is given a set of logging functions that log information before and after the execution of each Michelson expression.  This traced interpreter is also used for regression testing within each Tezos protocol update.

### Second pass 

However we felt that requiring users to run the entirety of any contracts that need to be debugged would not offer a good user experience.  What if you just want to examine the first few lines of execution?  What if you do not want to create a large trace file on disk?  Is there a middle ground where one can step in an incremental fashion whilst also recording the trace but only if requested, say to allow for backwards stepping?

Fortunately the traced interpreter can be parameterised by a user-defined [logger](https://gitlab.com/tezos/tezos/-/blob/master/src/proto_014_PtKathma/lib_protocol/script_typed_ir.mli#L1280) and so one can arrange matters such that execution can pause on exit from each Michelson expression whilst also recording the state of the program at each pause. 

This then is the approach taken in the ```weevil``` stepping tool.

| Parent service                      |                   | Child process                       | 
|  ---:                               | :---:             | :---                                | 
| ```$ weevil backend PORT ```        | .... spawns ....> | ```$ weevil stepper FILE```         |
|*service - listens on localhost:PORT to DAP*  |                   |  *stdin/out connected to parent*          |
|                                     | ....  step  ....> | *logger unpause then pause*                         |
|                                     | <.... state ....  | *logger send log*                          |
|                                     |       ...         |                                     |
|                                     | ....  step  ....> |                                     |
|                                     | <.... state ....  |                                     |
|                                     | .... exit  ....>  |                                     |

### Metaphor 

In terms of where this part lies in a future debugging tech stack for the Tezos ecosystem the intention is that it becomes a sort of equivalent to ```ptrace``` using ```sigtraps``` to step through machine code.

The metaphor then is Michelson instead of machine code, blocking the child process instead of ```sigtraps``` and ```mdb``` instead of ```ptrace``` .

## The MDB backend service

The ```weevil``` provides a backend [conduit](https://github.com/mirage/ocaml-conduit) TCP service for each supported version of the Tezos protocol:

```sh
$ weevil_mdb_016 backend --help 

WEEVIL-BACKEND(1)                Weevil Manual               WEEVIL-BACKEND(1)



NAME
       weevil-backend - Run the debugger service locally for the Weevil DAP
       service

SYNOPSIS
       weevil backend [OPTION]… [PORT]

DESCRIPTION
       Run the debugger service locally for the Weevil DAP service

ARGUMENTS
       PORT
           The port that the debugger svc will use for IO. If not given
           defaults to 9001
```

Its job is to mediate between the debug adapter service and the stepper.  The backend service can

* spawn the Michelson stepper as a child process, 
* step it forward one executable epression at a time
* retrieve log records of the state of the Michelson stack and gas levels at each Michelson code location,
* terminate the child process

On startup the stepper compiles and typechecks the specified contract, storage and parameter values.  It also verifies that the provided entrypoint exists or uses the default entrypoint if this is not specified.

The stepper pauses at the beginning of the contract execution and awaits the instruction from the parent service to step.

When the backend receives the ```next``` instruction from the debug adapter it passes that message on to the stepper (parent and child communicate via stdio channels) and the stepper takes one step then pauses again.  The stepper will log any new records back to the backend service on each step too.

When the debug adapter requests stack-frame information the backend service retreives the latest log messages that it has received from the stepper and passes them back to the adapter.

When the debug adapter requests to stop the debugging session the backend service terminates the child process.

TODO clean up of child process when it reaches the end of the contract

## Interacting with Octez 

The ```weevil_mdb_XXX``` tool directly reuses the Octez Michelson interpreter in OCaml.  We do this to give users confidence that the debugger output is as close to 'real-life' execution as possible.  It should also make it easier to keep up with the fast pace of Tezos protocol development.

All of the Octez interactions are kept inside the stepper child process.  The stepper uses the [mockup](https://tezos.gitlab.io/user/mockup.html) mode to allow for fully-local execution of contracts.  This mode of operation is augmented too with a special implementation of the [traced interpreter](https://gitlab.com/tezos/tezos/-/blob/master/src/proto_014_PtKathma/lib_plugin/RPC.ml#L468) that can pause execution of Michelson at each code location (c.f. the ```mdb``` [traced interpreter](../bin/weevil_mdb_016/src/mdb_traced_interpreter.mli)). 

TODO move to using the file backed mockup mode.

## Next steps 

### Breakpoints

Simple breakpoints at Michelson code locations can be implemented if the current basic ```pause/step``` state machine is embellished with an extra ```continue``` state.

In the ```continue ``` state the stepper should not pause after each executable expression but only when it arrives at a code location that has a breakpoint set.  At this point it enters the ```paused``` state and can be either ```stepped``` or ```continued``` as necessary.  Note that it could still log records for each code location to allow for backwards stepping (to be implemented at a later stage).

### Step in/out/over 

It's debatable whether we would want to support stepping in or over things at the Michelson level, it may be better to defer this functionality to when the ```weevil``` can be connected to other higher-level languages.  However it is the case that Michelson supports things that can be 'stepped into' e.g. ```lambdas``` and operations that jump the execution frame deeper into the Michelson stack.  It may be worth considering in the future.

### Other ideas

* Support multiple Tezos protocol versions c.f. dune [virtual-libraries](https://dune.readthedocs.io/en/stable/variants.html)
* Debug adapter/backend state management with [Irmin](https://irmin.org/)
* [DWARF](https://en.wikipedia.org/wiki/DWARF) file writer for e.g. [Ligo](https://ligolang.org/), [DWARF](https://en.wikipedia.org/wiki/DWARF) file reader for the ```weevil```

