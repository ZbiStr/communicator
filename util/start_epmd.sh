#!/bin/bash

erl -sname `date  +%s%N` -noshell -eval 'halt().'
