# Weevil

A debugger for the [Tezos](https://tezos.com/) smart contract language [Michelson](https://tezos.gitlab.io/active/michelson.html).

Implemented in [OCaml](https://ocaml.org/).

Hooks straight into the [Octez](https://gitlab.com/tezos/tezos) Michelson interpreter.

Wraps everything up in the [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/overview).

Please see the [Developer Setup](#developer-setup) below to get started.

# Thanks

Many thanks to the [Tezos Foundation](https://tezos.foundation/) who are funding this work.

If you would also like to get involved through sponsorship please see [here](https://github.com/sponsors/KaroshiBee).

If you just want to chat please reach out on Twitter [@karoshibee](https://twitter.com/karoshibee) or [LinkedIn](https://www.linkedin.com/in/simon-parry-3528b0198).

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
Package weevil does not exist, create as a NEW package? [Y/n] Y
...lots of packages get installed
```
## Build 

The dune build tool should now be available, I usually leave it running in watch mode (from project root):

``` sh
$ dune build -w
```

## Run

To run locally in Emacs you will need to do three things:

## Step 1 - load the dap-weevil extension to [dap-mode](https://github.com/emacs-lsp/dap-mode)

In Emacs open the [dap-weevil](./emacs/dap-weevil.el) elisp file and evaluate the elisp
```elisp
M-x eval-buffer
```

This should make available five example dap-debug sessions that can be run/debugged with the weevil.  

These five setups correspond to the five examples found in the [weevil source](./examples/) which are taken from the five examples used in the [opentezos](https://opentezos.com/michelson/examples/) website.

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

## Running other Michelson files

You will need to run 
```elisp
M-x dap-debug-edit-template
```
and choose one of the five setups to copy - it doesn't matter which one.  

Edit the ```script_filename```, ```storage``` and ```parameter``` string fields to the filename, storage and input parameter values you want.

Dont forget to also change the ```name``` field and have it register under that same new name.

Now when you next run ```M-x dap-debug``` you should see that new setup as one of the choices to run.

## Logging 

Currently we recommend running the adapter with the ```-v -v``` logging level (debug) to have full visibility of the message passing and sequencing of events.

Note also that by default the backend debugger service logs to a file called ```ROOT/weevil_mdb.log```. 

Please look in there for extra information if something is not running as expected.  It is possible that some errors in the backend do not get propagated back to the adapter (and on to the UI).  Please raise an issue if this is the case. 

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

# Opam Install 

WARN under heavy development - it is recommended to follow [Developer Setup](#developer-setup) above.

However an early preview version is available on opam; just do the usual 

```sh
$ opam install weevil
```

With this version you can run the ```weevil``` command to have a preview of the functionality, to obtain the full functionality please use the [Developer Setup](#developer-setup). 

# Documentation

* Notes on [messaging](./docs/messaging.md) including how to use the ```weevil dap-gen``` tool to auto-generate OCaml code for all the DAP message types,

* Notes on the ```weevil``` [Michelson stepper](./docs/stepping.md),

* Notes on the ```weevil``` [debug adapter](./docs/adapter.md).

# Demo
Pretty early-stage; here's a demo of an early prototype Emacs integration:

![weevil-demo](https://user-images.githubusercontent.com/411653/189655933-4a4ceb7a-21a5-4b0c-a5c1-59fc53e568e4.gif)


