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
	exec erl -pa $(PWD)/apps/zerolog/ebin -pa $(PWD)/deps/*/ebin -setcookie zerolog -name zerolog@127.0.0.1 -config $(PWD)/apps/zerolog/priv/app.config -s $(APP)

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

# ----------------------------------------------------------------------
#                       DIALYZER SUPPORT
#                       ---------------
APPS = kernel stdlib sasl erts ssl tools mnesia os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key eunit syntax_tools compiler ./deps/*/ebin
COMBO_PLT = .dialyzer.plt

check_plt: debug
	@dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS)

build_plt: debug
	@dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS)

dialyzer: debug
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	@dialyzer apps/zerolog/ebin --plt $(COMBO_PLT) \
		-Wno_return -Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		#-Wbehaviours \
		-Wunderspecs

cleanplt:
	@echo 
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo 
	sleep 5
	rm $(COMBO_PLT)
