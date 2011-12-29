ERL ?= erl
APP := zerolog

.PHONY: deps

all: deps compile

compile:
	@./rebar compile

debug:
	@./rebar debug_info=1 compile

deps:
	@./rebar get-deps

app:
	@./rebar compile skip_deps=true

webstart: app
	exec erl -pa $(PWD)/apps/zerolog/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -name zerolog@127.0.0.1 -config $(PWD)/apps/zerolog/priv/app.config -s $(APP)

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test:
	@./rebar compile skip_deps=true eunit

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

##
## Release targets
##
rel: deps
	./rebar compile generate

relclean:
	rm -rf rel/zerolog
