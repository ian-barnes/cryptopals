.PHONY: check build format test run clean

check:
	dune build @check

build: check
	dune build @default

format:
	dune build @fmt --auto-promote

test: build
	dune build @runtest

run: build
	dune exec main

clean:
	dune clean
