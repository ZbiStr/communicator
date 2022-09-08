#!/bin/bash
cp server_config.txt _build/default/lib/communicator/src/
cd _build/default/lib/communicator/src
erl -compile communicator client
erl -eval 'communicator:start_link().'
