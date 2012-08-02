ERL ?= erl
APP := zerolog
REL := dev
PREVIOUS_RELEASE = `ls -t1dm1 rel/$(APP)* | head -n1 | sed 's:rel/::'`
LATEST_RELEASE = `ls -t1dm1 /distfiles/$(APP)/$(REL)/releases/$(APP)* | head -n1 | sed 's:/distfiles/$(APP)/$(REL)/releases/::' | sed 's:.tar.gz::'`
REBAR = ./rebar

.PHONY: deps

all: deps compile

compile:
	@$(REBAR) compile

debug:
	@$(REBAR) debug_info=1 compile

deps:
	@$(REBAR) get-deps

app:
	@$(REBAR) compile skip_deps=true

webstart: app
	exec erl -pa $(PWD)/apps/$(APP)/ebin -pa $(PWD)/deps/*/ebin -setcookie zerolog -name zerolog@127.0.0.1 -config $(PWD)/apps/$(APP)/priv/app.config -s $(APP)

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

test:
	@$(REBAR) compile skip_deps=true eunit

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

##
## Release targets
##
rel: deps
	@$(REBAR) compile generate
	mv rel/$(APP) rel/$(APP)_`git describe --always --tags`

relup: deps
	@$(REBAR) compile generate
	@$(REBAR) generate-appups previous_release=$(PREVIOUS_RELEASE)
	@$(REBAR) generate-upgrade previous_release=$(PREVIOUS_RELEASE)
	@mv rel/$(APP) rel/$(APP)_`git describe --always --tags`

deploy:
ifneq ($(wildcard /distfiles/$(APP)/$(REL)/releases/$(APP)*),)
	@cp /distfiles/$(APP)/$(REL)/releases/$(LATEST_RELEASE).tar.gz rel/
	@cd rel; tar zxf $(LATEST_RELEASE).tar.gz; mv $(APP) $(LATEST_RELEASE)
	@$(REBAR) compile generate
	@$(REBAR) generate-appups previous_release=$(LATEST_RELEASE)
	@$(REBAR) generate-upgrade previous_release=$(LATEST_RELEASE)
	@mv rel/$(APP)_`git describe --always --tags`.tar.gz /distfiles/$(APP)/$(REL)/upgrades/
	@echo `git describe --always --tags` > /distfiles/$(APP)/$(REL)/upgrades/latest
else
	@$(REBAR) compile generate
endif
	@cd rel;tar -pczf $(APP).tar.gz $(APP)
	@mv rel/$(APP).tar.gz /distfiles/$(APP)/$(REL)/releases/$(APP)_`git describe --always --tags`.tar.gz
	@ln -sf /distfiles/$(APP)/$(REL)/releases/$(APP)_`git describe --always --tags`.tar.gz /distfiles/$(APP)/$(REL)/releases/latest.tar.gz

relclean:
	rm -rf rel/$(APP)*

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
