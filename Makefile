REBAR=$(shell which rebar3)
APP=$(shell sed -n '/application,/{s/^.*, //; s/,.*$$//; p; q}' src/*.app.src)

ifeq ($(REBAR),)
	$(error ERROR: rebar3 not found in PATH)
endif

.PHONY: help all compile clean distclean info test run check docs publish cut bump-version retire-version show-versions

help:
	@echo "egit - Erlang NIF bindings for libgit2"
	@echo ""
	@echo "Available targets:"
	@echo "  all              Compile the project (default)"
	@echo "  compile          Compile the project"
	@echo "  clean            Remove compiled artifacts"
	@echo "  distclean        Remove all build artifacts and _build directory"
	@echo "  test             Run unit tests with eunit"
	@echo "  check            Run Dialyzer type checking"
	@echo "  docs             Generate documentation with ex_doc"
	@echo "  run              Start interactive Erlang shell"
	@echo "  info             Show compiler information"
	@echo ""
	@echo "Release management:"
	@echo "  publish          Publish package to Hex"
	@echo "  cut              Create release on Hex"
	@echo "  bump-version     Increment patch version and commit"
	@echo "  retire-version   Retire a version on Hex"
	@echo "  show-versions    Show all versions on Hex"
	@echo ""
	@echo "Examples:"
	@echo "  make test        Run all unit tests"
	@echo "  make compile     Build the NIF library"
	@echo "  make help        Show this help message"

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

bump-version:
	@FILE=$$(ls -1 src/*.app.src | head -n1); \
	CURRENT=$$(grep -m1 '{vsn,' $$FILE | sed -E 's/.*"([0-9]+\.[0-9]+\.[0-9]+)".*/\1/'); \
	MAJOR=$$(echo $$CURRENT | cut -d. -f1); \
	MINOR=$$(echo $$CURRENT | cut -d. -f2); \
	PATCH=$$(echo $$CURRENT | cut -d. -f3); \
	NEW=$$(echo "$${MAJOR}.$${MINOR}.$$((PATCH + 1))" | tr -d '\n'); \
	echo "Bumping version from $${CURRENT} to $${NEW}"; \
	sed -i "s/{vsn, \"$${CURRENT}\"}/{vsn, \"$${NEW}\"}/" $$FILE; \
	echo "Changed: {vsn, \"$${CURRENT}\"} -> {vsn, \"$${NEW}\"}"; \
	echo ""; \
	read -p "Commit this change? [Y/n] " -n 1 -r || true; \
	echo ""; \
	if [[ $$REPLY =~ ^[Yy]$$ ]] || [[ -z $$REPLY ]]; then \
		git commit -am "Bump version to $${NEW}"; \
	else \
		echo "Aborted. Reverting rebar.config..."; \
		git checkout rebar.config; \
		exit 1; \
	fi

retire-version: VSN=$(if $(version),$(version),$(shell mix hex.info $(APP) | grep "^Releases:" | sed 's/Releases: //; s/, /\n/g' | sed '/retired/d; /\.\.\./d' | sed -n '$$p'))
retire-version:
	@if [ -z "$(VSN)" ]; then \
		echo "$(APP): no stale versions were found on Hex"; \
	else \
		echo "Retiring version $(VSN) of $(APP) on Hex..."; \
		rebar3 hex retire erlalign $(VSN) deprecated --message "Deprecated"; \
	fi

show-versions:
	@mix hex.info $(APP) | grep "^Releases:" | sed 's/Releases: //; s/, /\n/g' | sed '/retired/d; /\.\.\./d'
