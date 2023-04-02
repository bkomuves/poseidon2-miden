
Miden macro-assembler
---------------------

This a quick prototype to add compile-time code generation to Miden assembly,
similarly as `circom` allows compile time computation, for loops, branches, etc.

It was used to implement the Poseidon2 hash function for Miden.

### Quickstart

Install Haskell using [`ghcup`](https://www.haskell.org/ghcup/)

Then build the project using

    $ cabal build
    $ cabal install

Then you can use it like this:

    $ miden-macro-assembler poseidon2.meta

This will generate a Miden assembly source code file `poseidon2.masm`.
