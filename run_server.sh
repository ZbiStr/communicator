#!/bin/bash
cp server_config.txt _build/default/lib/communicator/ebin/
cp prompts _build/default/lib/communicator/ebin/

echo "Select your language"
echo "pl -polski"
echo "en -english"
read -p "Option: " lang

if [ "$lang" = "en" ]; then
	erl -pa '_build/default/lib/communicator/ebin' -eval 'communicator:start_link(en).'
fi

if [ "$lang" = "pl" ]; then
	erl -pa '_build/default/lib/communicator/ebin' -eval 'communicator:start_link(pl).'
fi

echo "Wrong language"
$SHELL

