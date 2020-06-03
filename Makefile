.PHONY: check deps build format test run clean

check:
	dune build @check
	@echo

deps:
	opam install . --deps-only --locked --working-dir

build: check
	dune build @default
	@echo

format:
	dune build @fmt --auto-promote
	@echo

test: build
	dune build @runtest
	@echo

run: build
	dune exec main
	@echo

clean:
	dune clean
