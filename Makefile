REBAR=`which rebar || ./rebar`

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

run:
	erl -pa ebin deps/*/ebin -s pusherl_api

test:
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

.PHONY: test clean run deps run
