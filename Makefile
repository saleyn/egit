REBAR=$(shell which rebar3)

ifeq ($(REBAR),)
	$(error ERROR: rebar3 not found in PATH)
endif

all: compile

compile clean:
	@$(REBAR) $@

test:
	$(REBAR) eunit

run:
	$(REBAR) shell

docs:
	$(REBAR) ex_doc

publish cut:
	$(REBAR) hex $@ -r hexpm $(if $(replace),--replace) $(if $(noconfirm),--yes)
