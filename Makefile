all: compile

get-deps:
	./rebar get-deps

compile: get-deps
	./rebar compile

clean:
	./rebar clean

release:
	./rebar generate

config:
	cp default.config.example default.config

