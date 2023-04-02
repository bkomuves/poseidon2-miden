
This is a Poseidon2 hash implementation in Miden assembly.

The implementation is (for now) hardcoded for the Goldilocks prime field
(Miden's native field) and t=8, that is, the permutation state is 8 field 
elements.

The implementation is compatible with the Rust reference implementation for 
Poseidon2: <https://github.com/HorizenLabs/poseidon2>

The sponge construction is not yet implemented, only the permutation and
two-to-one compression (which can be applied to construct a Merkle tree, 
for example).

The implementation is written using a meta-compiler (or macro-assembler), 
also developed from scratch from Saturday evening to Sunday dawn. This is
written in Haskell.

