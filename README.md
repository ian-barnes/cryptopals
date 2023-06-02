# Cryptopals

- Work in progress
- OCaml solutions to the [Cryptopals Crypto Challenges](https://cryptopals.com/)
- Completed Sets 1 & 2
- Done the first two exercises in Set 3, working on the next two...

## Workflow

- During development / experimentation, new code lives in `bin/main.ml`.
- As it improves, useful functions get abstracted out and moved into `lib/`.
- When a challenge is complete, the driver code ends up in `test/`.

## Setup

- The self-documenting `Makefile` here automates most of the tasks. Just type `make` to
  see a list of (dummy) targets and their documentation.
- You may want to create an Opam `switch` to isolate the OCaml environment from other
  OCaml projects on your system. If so, something like
  `opam switch create . ocaml-base-compiler.4.14.1` should do it.
- To install all the dependencies: `make deps`.
