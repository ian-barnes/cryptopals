# Cryptopals

- Work in progress
- OCaml solutions to the [Cryptopals Crypto Challenges](https://cryptopals.com/)
- Completed Sets 1 & 2

# Workflow

- During development / experimentation, new code lives in `bin/main.ml`.
- As it improves, useful functions get abstracted out and moved into `lib/`.
- When a challenge is complete, the driver code ends up in `test/`.
