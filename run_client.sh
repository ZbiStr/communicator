#!/bin/bash
cp util/run.escript _build/default/lib/communicator/ebin/
cd _build/default/lib/communicator/ebin

echo "Select your language"
echo "pl -polski"
echo "en -english"
read -p "Option: " lang

if [ "$lang" = "en" ]; then
	./run.escript en
fi

if [ "$lang" = "pl" ]; then
	./run.escript pl
fi

$SHELL
