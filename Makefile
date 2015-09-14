REBAR?=./rebar

all: compile
	erl -pa `pwd`/ebin -pa `pwd`/deps/**/ebin -s erlgraph_app

compile:
	$(REBAR) get-deps compile
	$(REBAR) compile

clean:
	$(REBAR) clean

.PHONY: all deps clean
