.PHONY: help check deps build format test slow run clean

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort | awk 'BEGIN {FS = ":.*?## "}; \
		{printf "\033[36m%-8s\033[0m %s\n", $$1, $$2}'

check:  ## Type check
	dune build @check
	@echo

deps:  ## Refresh dependencies
	opam install . --deps-only --locked --working-dir

build: check  ## Full compilation
	dune build @default
	@echo

format:  ## Reformat code
	dune build @fmt --auto-promote
	@echo

test: build  ## Run fast unit tests
	dune build @runtest
	@echo

slow: build  ## Run all unit tests
	dune build @slowtests
	@echo

run: build  ## Run
	dune exec main
	@echo

clean:  ## Clean workspace
	dune clean
