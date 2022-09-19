#!/bin/bash
cp util/run.escript _build/default/lib/communicator/ebin/
cd _build/default/lib/communicator/ebin

echo "Select your language"
echo "pl -polski"
echo "en -english"
read -p "Option: " lang

./run.escript $lang

$SHELL
