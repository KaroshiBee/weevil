#!/usr/bin/env sh

echo ""
echo "** These aliases are just for dev/test purposes **"
echo "** they will only work from the weevil project root directory **"
echo ""

alias weevil='dune exec -- ./src/main.exe'
echo "alias weevil"

alias weevil-messages='dune exec -- ./src/main.exe dap-gen messages ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_message && echo "output: $HOME/dap_message.ml"'
echo "alias weevil-messages"

alias weevil-commands='dune exec -- ./src/main.exe dap-gen commands ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_commands && echo "output: $HOME/dap_commands.ml[i]"'
echo "alias weevil-commands"

alias weevil-events='dune exec -- ./src/main.exe dap-gen events ./schema/debugAdapterProtocol-1.56.X.json $HOME/dap_events && echo "output: $HOME/dap_events.ml[i]"'
echo "alias weevil-events";
