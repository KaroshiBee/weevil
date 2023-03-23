# The ```dapper``` messages

## Introduction

The [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/overview) (henceforth DAP) describes a general method and message protocol for building debuggers.  A DAP frontend will send ```Request``` JSON messages to the DAP adapter which then performs that request action and responds with a ```Response``` JSON message, and optionally one or more ```Event``` JSON messages.

Microsoft publishes a machine-readable [schema](https://microsoft.github.io/debug-adapter-protocol/debugAdapterProtocol.json) describing all the message types and supporting objects.  This schema file is also checked in to the [weevil](../schema) codebase.

So the first thing a new adapter implementation needs is a way to consume and produce JSON messages of these types.

Given that there are so many message types and also that the schema doesn't change frequently we opted to auto-generate OCaml code to handle this.

The ```dap-gen``` CLI tool is used to re-generate the necessary OCaml files:

``` sh
$ dune exec -- weevil dap-gen --help
```
See below for more details.

## DAP message structure

As noted above, there are three message types: ```Request```, ```Response``` and ```Event```.  ```Requests``` and ```Responses``` have a related ```command``` value and ```Events``` have an ```event``` value.  All message types also carry some sort of data, and this can be required or optional.  

The command and event values are basically enumerations and the data content types are typically JSON objects that themselves contain other objects/lists/enums etc.

This leads to the following OCaml representation (c.f. [requests](../src/dapper/dap_request_message.ml) & [responses](../src/dapper/dap_response_message.ml) & [events](../src/dapper/dap_event_message.ml) ):

``` ocaml

type ('cmd, 'args, 'presence) Dap_request_message.t

type ('cmd, 'body, 'presence) Dap_response_message.t

type ('event, 'body, 'presence) Dap_event_message.t

```
Here ```'cmd``` and ```'event``` relate to the ```command``` and ```event``` enums described previously.  The ```'args``` and ```'body``` parameters are the contents of the message, and the ```'presence``` parameter determines whether this content is required or optional.

On processing the JSON schema the auto-generation tool can lock in the correct types of each message kind thus eliminating all kinds of bugs that could arise due to incorrect message construction.  For example 

``` ocaml
( Dap_events.stopped, StoppedEvent_body.t, Presence.req ) Dap_event_message.t

```
is a ```stopped``` event message that has some required content data of type ```StoppedEvent_body.t```,

where-as 

``` ocaml
( Dap_commands.initialize, Capabilities.t option, Presence.opt ) Dap_response_message.t

```
is an ```initialize``` response message that has optional content data of type ```Capabilities.t```.

Editor tooling like [Merlin](https://github.com/ocaml/merlin) and [Tuareg](https://github.com/ocaml/tuareg) also helps tremendously as each OCaml message type knows what contents it needs. 

## The ```dap-gen``` tool

The ```dap-gen``` tool will generate the command and event enumerations (c.f. [commands](../src/dapper/dap_commands.mli)/[impl](../src/dapper/dap_commands.ml) and [events](../src/dapper/dap_events.mli)/[impl](../src/dapper/dap_events.ml) and also the [message types](../src/dapper/dap_messages.ml) with supporting objects).

Simply run it with the required output type, JSON schema and filename:

``` sh
$ dune exec -- weevil dap-gen messages ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_message
```
will output the dap_message.ml file in your $HOME dir.  Similarly, 

``` sh
$ dune exec -- weevil dap-gen commands ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_commands
```
Will generate dap_commands.ml and dap_commands.mli in $HOME, and 
``` sh
$ dune exec -- weevil dap-gen events ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_events
```
will generate dap_events.ml and dap_events.mli in $HOME.

If you are happy with the generated code files then move/copy the files into the dapper library

``` sh
$ cp dap_*.ml* ./src/dapper/
```

NOTE The dapper library is expecting these files to be named as above.


## Notes

The Weevil uses the same JSON encoder lib as [Octez](https://tezos.gitlab.io/), namely [data-encoding](https://gitlab.com/nomadic-labs/data-encoding/-/blob/master/src/tutorial.md).

All generated message types and object types have a similar structure:

- they all have a constructor called ```make``` that takes named parameters for construction.  Named parameters are required/optional as stated in the JSON schema using the standard OCaml syntax (```~``` and ```?```) e.g.

``` ocaml
let make ~a_required_parameter ?an_optional_parameter ... () =
    ...etc 
```

- Request/Response/Event types also have a ```make_opt``` constructor for creating message types with optional content data,

- due to the use of named parameters everywhere all of these constructors have to end in () i.e. unit - this is an OCaml artifact related to named parameters and currying,

- they all have a ```Data_encoding.t``` encoder named ```enc```,

- Request/Response/Event types also have an ```enc_opt``` encoder to encode/decode message types with optional content data,

- they all have (fairly simple) [qcheck](https://github.com/c-cube/qcheck/) generators for property based testing 


