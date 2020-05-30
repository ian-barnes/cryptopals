.PHONY: build format test run clean

build:
	dune build @check @default

format:
	dune build @fmt --auto-promote

test: build
	dune build @runtest

run: build
	dune exec main

clean:
	dune clean
