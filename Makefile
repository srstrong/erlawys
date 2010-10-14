.PHONY: deps

all:  compile

compile: deps
	./rebar compile

deps:
	@./rebar get-deps

edoc:
	./rebar doc

test:
	./rebar eunit

clean:
	./rebar clean

dialyzer:
	./rebar analyze

