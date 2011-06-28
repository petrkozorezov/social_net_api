all: compile

compile:
	rebar compile

clean:
	rebar clean

release:
	rebar generate

config:
	cp default.config.example default.config

