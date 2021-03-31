.PHONY: help check deps build format test slow run clean

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort | awk 'BEGIN {FS = ":.*?## "}; \
		{printf "\033[36m%-8s\033[0m %s\n", $$1, $$2}'

check:  ## Type check
	dune build @check

deps:  ## Refresh dependencies
	opam install . --deps-only --locked --working-dir

build: check  ## Full compilation
	dune build @default

format:  ## Reformat code
	dune build @fmt --auto-promote

test: build  ## Run fast unit tests
	dune build @runtest

slow: build  ## Run all unit tests
	dune build @slowtests

run: build  ## Run
	dune exec main

docs:  ## Build documentation
	dune build @doc @doc-private

clean:  ## Clean workspace
	dune clean
