# Stepping Through Michelson Code

## Introduction 

In this document we describe the approach taken to allow the ```weevil``` debug adapter to step through Michelson code one executable expression at a time.

The code for this implementation is found in the [mdb](https://github.com/KaroshiBee/weevil/tree/main/src/mdb) submodule of the ```weevil``` tool.

### First pass

One simple way to implement stepping for a given contract and entrypoint would be to run the whole thing from start to finish with the desired input and storage and record a trace of everything that occured.  This trace could then form the basis of a debugging system that allows one to step back and forth at will.

This was in fact the approach taken for the very first prototype of the ```weevil``` tool.  

It is also a mode of execution supported by the ```octez-client``` in mockup mode with the ```--trace-stack``` option.  In this mode the main Michelson interpreter is given a set of logging functions that log information before and after the execution of each Michelson expression.  This traced interpreter is also used for regression testing within each Tezos protocol update.

### Second pass 

However we felt that requiring users to run the entirity of any contracts that need to be debugged would not offer a good user experience.  What if you just want to examine the first few lines of execution?  What if you do not want to create a large trace file on disk?  Is there a middle ground where one can step in an incremental fashion whilst also recording the trace but only if requested, say to allow for backwards stepping?

Fortunately the traced interpreter can be parameterised by a user-defined [logger](https://gitlab.com/tezos/tezos/-/blob/master/src/proto_014_PtKathma/lib_protocol/script_typed_ir.mli#L1280) and so one can arrange matters such that execution can pause on exit from each Michelson expression whilst also recording the state of the program at each pause. 

This then is the approach taken in the ```weevil``` stepping tool.

## The MDB stepper

## The MDB backend service

## Querying the state of the program 

## Next steps 

breakpoints

step in/out 


