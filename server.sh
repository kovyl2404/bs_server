#! /bin/bash
[ -z $ENV ] && ENV=staging 
erl -config $ENV -pa deps/*/ebin -pa apps/*/ebin/ -s lager -s game_server 
