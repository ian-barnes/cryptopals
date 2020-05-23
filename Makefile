.PHONY: format test run

compile:
	dune build @check @default
format:
	dune build @fmt --auto-promote

test: compile
	dune build @runtest

run: compile
	dune exec main
