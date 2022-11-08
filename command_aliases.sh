#!/usr/bin/env sh

alias weevil='dune exec -- ./src/main.exe'
alias weevil-messages='dune exec -- ./src/main.exe dap-gen messages ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_message'
alias weevil-commands='dune exec -- ./src/main.exe dap-gen commands ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_commands'
alias weevil-events='dune exec -- ./src/main.exe dap-gen events ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_events'
