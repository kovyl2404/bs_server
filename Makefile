.PHONY: get-deps compile int_tests unit_tests all_tests release clean

release: compile
	rebar generate

get-deps:
	./rebar get-deps

compile: get-deps
	./rebar compile


int_tests: compile
	./rebar eunit -v skip_deps=true apps=inttests,database

unit_tests: compile
	./rebar eunit -v skip_deps=true apps=game_lobby,game_server

all_tests: unit_tests, int_tests


clean:
	rebar clean
