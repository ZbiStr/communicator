#!/bin/bash
cp server_config.txt _build/default/lib/communicator/ebin/
cd _build/default/lib/communicator/ebin
erl -eval 'communicator:start_link().'
