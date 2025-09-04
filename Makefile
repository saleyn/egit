REBAR=$(shell which rebar3)

ifeq ($(REBAR),)
	$(error ERROR: rebar3 not found in PATH)
endif

all: compile

compile clean:
	@$(REBAR) $@

distclean: clean
	@rm -fr _build

info:
	@make -C c_src $@

test:
	$(REBAR) eunit

run:
	$(REBAR) shell

check:
	$(REBAR) dialyzer

docs:
	$(REBAR) ex_doc

publish cut:
	$(REBAR) hex $@ -r hexpm $(if $(replace),--replace) $(if $(noconfirm),--yes)
