# Weevil

A debugger for Michelson in OCaml.

Hooks straight into the [Octez](https://gitlab.com/tezos/tezos) Michelson interpreter.

Wraps everything up in the [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/overview).

# Setup

Currently under heavy development so to play along please check out the source code and build locally.

## Requirements 

Developed/Tested on Ubuntu 22.04.1 LTS with opam version 2.1.2, you will need to make sure that ocaml-base-compiler.4.14.0 is available:

``` sh
$ opam switch list-available | grep base-compiler | grep Official

    ocaml-base-compiler                    4.12.0                                 Official release 4.12.0
    ocaml-base-compiler                    4.12.1                                 Official release 4.12.1
    ocaml-base-compiler                    4.13.0                                 Official release 4.13.0
    ocaml-base-compiler                    4.13.1                                 Official release 4.13.1
>>> ocaml-base-compiler                    4.14.0                                 Official release 4.14.0 <<<
```
## Check out & install dependencies

``` sh
$ git checkout https://github.com/wyn/weevil.git
$ cd weevil/
$ opam switch create . ocaml-base-compiler.4.14.0 
Package tezos-weevil does not exist, create as a NEW package? [Y/n] Y
...lots of packages get installed
```
## Build 

The dune build tool should now be available, I usually leave it running in watch mode (from project root):

``` sh
$ dune build -w
```

## Test 

To run the tests simply do (from project root):

``` sh
$ dune runtest
```
Tests currently consist of [Alcotest](https://github.com/mirage/alcotest) unit tests and [expect](https://github.com/janestreet/ppx_expect) tests.
Coverage statistics can be determined using the [bisect ppx](https://github.com/aantron/bisect_ppx#Dune) tool.  

As described in the link, you can perform a full test with statistics gathering using: 

``` sh
# remove previous data
$ find . -name '*.coverage' | xargs rm -f

# run tests with instrumentation
$ dune runtest --instrument-with bisect_ppx --force
```

and then look at a short summary:

``` sh
$ bisect-ppx-report summary
```

or generate a full report:

``` sh
$ bisect-ppx-report html
```

The html bundle is written to the _coverage directory and can be served with a simple web server a la:

``` sh
$ python -m http.server --directory _coverage/
Serving HTTP on 0.0.0.0 port 8000 (http://0.0.0.0:8000/) ...
```

# Auto-generating code 

The [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/overview) describes a general method and message protocol for building debuggers.  A DAP frontend will send ```Request``` JSON messages to the DAP adapter which then performs that request action and responds with a ```Response``` JSON message, and optionally one or more ```Event``` JSON messages.

Microsoft publishes a machine-readable [schema](https://microsoft.github.io/debug-adapter-protocol/debugAdapterProtocol.json) describing all the message types and supporting objects.  This schema file is also checked in to the Weevil codebase (c.f. ./schema).

So the first thing a new adapter implementation needs is a way to consume and produce JSON messages of these types.

Given that there are so many message types and also that the schema doesn't change frequently we opted to auto-generate OCaml code to handle this.

The ```dap-gen``` CLI tool is used to re-generate the necessary OCaml files:

``` sh
$ dune exec -- ./src/main.exe dap-gen --help
```
## Aside - DAP message structure

As noted above, there are three message types: Request, Response and Event.  Requests and Responses have a related ```command``` value and Events have an ```event``` value.  All message types also carry some sort of data, and this can be required or optional.  The command and event values are basically enumerations and the data content types are typically JSON objects that themselves contain other objects/lists/enums etc.

This leads to the following OCaml representation (c.f. in src/dapper/ dap_request.mli & dap_response.mli & dap_event.mli):

``` ocaml

type ('cmd, 'args, 'presence) Dap_request.t

type ('cmd, 'body, 'presence) Dap_response.t

type ('event, 'body, 'presence) Dap_event.t

```
Here 'cmd and 'event relate to the ```command``` and ```event``` enums desribed previously.  The 'args and 'body parameters are the contents of the message, and the 'presence determines whether the content is required or optional.

On processing the JSON schema the auto-generation tool can lock in the correct types of each message kind thus eliminating all kinds of bugs that could arise due to incorrect message construction.  Editor tooling like [Merlin](https://github.com/ocaml/merlin) and [Tuareg](https://github.com/ocaml/tuareg) also helps tremdously as each message type knows what innards it needs. 

## The ```dap-gen``` tool

The ```dap-gen``` tool will generate the command and event enumerations (c.f. src/dapper/dap_commands.ml* and src/dapper/dap_events.ml*) and also the message types and supporting objects (c.f. src/dapper/dap_message.ml).

Simply run it with the required output type, JSON schema and filename:

``` sh
$ dune exec -- ./src/main.exe dap-gen messages ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_message
```
will output the dap_message.ml file in your $HOME dir.  Similarly, 

``` sh
$ dune exec -- ./src/main.exe dap-gen commands ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_commands
```
Will generate dap_commands.ml and dap_commands.mli, and 
``` sh
$ dune exec -- ./src/main.exe dap-gen events ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_events
```
will generate dap_events.ml and dap_events.mli.

## Notes

The Weevil uses the same JSON encoder lib as [Octez](https://tezos.gitlab.io/), namely [data-encoding](https://gitlab.com/nomadic-labs/data-encoding/-/blob/master/src/tutorial.md).

All generated message types and object types have a similar structure:

- they all have a constructor called ```make``` that takes named parameters for construction, parameters are required/optional as stated in the JSON schema,

- Request/Response/Event types also have a ```make_opt``` constructor to lock in the appropriate `presence type for the message data,

- due to the use of named parameters everywhere all constructors have to end in () i.e. unit - this is an OCaml artifact related to named parameters and currying,

- they all have an encoder named ```enc```,

- Request/Response/Event types also have an ```enc_opt``` encoder to lock in the appropriate `presence type for the message data,

- coming soon: qcheck generators for the auto-generated types 

# Demo
Pretty early-stage; here's a demo of an early prototype Emacs integration:

![weevil-demo](https://user-images.githubusercontent.com/411653/189655933-4a4ceb7a-21a5-4b0c-a5c1-59fc53e568e4.gif)


