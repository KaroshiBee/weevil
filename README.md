# Weevil

A debugger for Michelson in OCaml.

Hooks straight into the [Octez](https://gitlab.com/tezos/tezos) Michelson interpreter.

Wraps everything up in the [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/overview).

# Install 

WARN under heavy development - better to follow Developer Setup below

Using opam just do the usual 

```sh
$ opam install weevil
```

# Developer Setup

The Tezos Weevil is currently under heavy development so to play along please check out the source code and build locally.

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
$ git checkout https://github.com/karoshibee/weevil.git
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

## Run

To run locally in Emacs you will need to do three things:

## Step 1 - make Emacs aware of the weevil extension to [dap-mode](https://github.com/emacs-lsp/dap-mode)

In Emacs open the file in the weevil source ```ROOT/emacs/dap-weevil.el``` and evaluate the elisp
```elisp
M-x eval-buffer
```

This should make available five example dap-debug sessions that can be run/debugged with the weevil.  

These five setups correspond to the five examples found in the weevil source ```ROOT/examples/*.tz``` which are taken from the five examples used in the [opentezos](https://opentezos.com/michelson/examples/) website.

## Step 2 - run the adapter service in a separate process 

```sh
$ dune exec -- weevil adapter -v -v
```

This should set the adapter service running and listening to connections on the default port 9000.

## Step 3 - back in emacs run 
```elisp
M-x dap-debug
```

It should show you the five debug sessions from step 1, choose the one you want to debug.

The debug session should start and you should see Emacs connecting with the adapter.

To step into the Michelson code do
```elisp
M-x dap-next
```
The example code file should be presented (if you aren't already viewing it in a buffer) with the current line and cursor point highlighted.

To see a standard debugging window setup do
```elisp
M-x dap-ui-show-many-windows
```
On the right-hand side you should be able to drill down into the Michelson stack & gas along with the input parameter values and initial storage.

Repeated use of 
```elisp
M-x dap-next 
```
will alllow you to continue to step through the Michelson code one instruction at a time.

To disconnect use
```elisp
M-x dap-disconnect
```

NOTE currently that is all the UI instructions that the weevil will respond to.

## Test 

To run the all the tests simply do (from project root):

``` sh
$ dune runtest -f
```
Tests currently consist of [Alcotest](https://github.com/mirage/alcotest) unit/qcheck tests, [expect](https://github.com/janestreet/ppx_expect) tests and [cram](https://dune.readthedocs.io/en/stable/tests.html) tests.

The qcheck tests can take some time to run through so to just run the quick tests do

```sh
$ ALCOTEST_QUICK_TESTS=1 dune runtest -f
```

Coverage statistics can be determined using the [bisect ppx](https://github.com/aantron/bisect_ppx#Dune) tool.  

As described in the link, you can perform a full test with statistics gathering using: 

``` sh
# make a dir to keep the bisect_ppx output 
$ mkdir _bisect

# remove previous data
$ find . -name '*.coverage' | xargs rm -f

# run tests with instrumentation
$ BISECT_FILE=$PWD/_bisect/bisect dune runtest --force --instrument-with bisect_ppx
```

The BISECT_FILE env var is used to ensure that the cram test [sandboxing](https://dune.readthedocs.io/en/stable/concepts.html#dune-action-plugin) does not clean up (delete) the coverage data for those same cram tests.  One can then look at a summary:

``` sh
$ bisect-ppx-report summary --per-file --coverage-path $PWD/_bisect
```

or generate a full report:

``` sh
$ bisect-ppx-report html --tree --coverage-path $PWD/_bisect
```

The html bundle is written to the _coverage directory by default; the contents of which can be served with a simple web server a la:

``` sh
$ python -m http.server --directory _coverage/
Serving HTTP on 0.0.0.0 port 8000 (http://0.0.0.0:8000/) ...
```

This html report can then be viewed with your web browser at http://localhost:8000

# Auto-generating code 

The [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/overview) describes a general method and message protocol for building debuggers.  A DAP frontend will send ```Request``` JSON messages to the DAP adapter which then performs that request action and responds with a ```Response``` JSON message, and optionally one or more ```Event``` JSON messages.

Microsoft publishes a machine-readable [schema](https://microsoft.github.io/debug-adapter-protocol/debugAdapterProtocol.json) describing all the message types and supporting objects.  This schema file is also checked in to the Weevil codebase (c.f. ```ROOT/schema```).

So the first thing a new adapter implementation needs is a way to consume and produce JSON messages of these types.

Given that there are so many message types and also that the schema doesn't change frequently we opted to auto-generate OCaml code to handle this.

The ```dap-gen``` CLI tool is used to re-generate the necessary OCaml files:

``` sh
$ dune exec -- weevil dap-gen --help
```

## Aside - DAP message structure

As noted above, there are three message types: Request, Response and Event.  Requests and Responses have a related ```command``` value and Events have an ```event``` value.  All message types also carry some sort of data, and this can be required or optional.  The command and event values are basically enumerations and the data content types are typically JSON objects that themselves contain other objects/lists/enums etc.

This leads to the following OCaml representation (c.f. in ```ROOT/src/dapper/dap_request_message.mli``` & ```dap_response_message.mli``` & ```dap_event_message.mli```):

``` ocaml

type ('cmd, 'args, 'presence) Dap_request_message.t

type ('cmd, 'body, 'presence) Dap_response_message.t

type ('event, 'body, 'presence) Dap_event_message.t

```
Here 'cmd and 'event relate to the ```command``` and ```event``` enums described previously.  The 'args and 'body parameters are the contents of the message, and the 'presence parameter determines whether this content is required or optional.

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

Editor tooling like [Merlin](https://github.com/ocaml/merlin) and [Tuareg](https://github.com/ocaml/tuareg) also helps tremdously as each OCaml message type knows what innards it needs. 

## The ```dap-gen``` tool

The ```dap-gen``` tool will generate the command and event enumerations (c.f. ```ROOT/src/dapper/dap_commands.ml*``` and ```ROOT/src/dapper/dap_events.ml*```) and also the message types and supporting objects (c.f. ```ROOT/src/dapper/dap_message.ml```).

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

- they all have a constructor called ```make``` that takes named parameters for construction.  Named parameters are required/optional as stated in the JSON schema using the standard OCaml syntax (~ and ?) e.g.

``` ocaml
let make ~a_required_parameter ?an_optional_parameter ... () =
    ...etc 
```

- Request/Response/Event types also have a ```make_opt``` constructor for creating message types with optional content data,

- due to the use of named parameters everywhere all of these constructors have to end in () i.e. unit - this is an OCaml artifact related to named parameters and currying,

- they all have a ```Data_encoding.t``` encoder named ```enc```,

- Request/Response/Event types also have an ```enc_opt``` encoder to encode/decode message types with optional content data,

- they all have (fairly simple) [qcheck](https://github.com/c-cube/qcheck/) generators for property based testing 

# Demo
Pretty early-stage; here's a demo of an early prototype Emacs integration:

![weevil-demo](https://user-images.githubusercontent.com/411653/189655933-4a4ceb7a-21a5-4b0c-a5c1-59fc53e568e4.gif)


