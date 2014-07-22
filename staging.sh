#! /bin/bash
 
erl -config staging -pa deps/*/ebin -pa apps/*/ebin/ -s lager -s game_server 
