# PECAC: A Parameterized Equivalence Checker for Ansatz Circuits

PECAC is an automated equivalence checker for parameterized quantum circuits written in OpenQASM 3.
A parameterized quantum circuit is a quantum circuit which one or more rotations has a symbolic parameter.
Two parameterized circuits are said to be equivalent if they agree on all assignments to their symbolic parameters.
Currently, PECAC supports exact equivalence and equivalence up-to global phase.

## Installing PECAC

The PECAC project is written in Haskell using `cabal` as a build system.
This mean that `cabal` must be [installed]() before building PECAC.
Once `cabal` has been installed, PECAC can be built by running the following commands.
```
cabal update
cabal new-build
```
Note that the [build configurations](https://downloads.haskell.org/~ghc/7.8.3/docs/html/users_guide/code-generators.html) use `llvm` as a compiler backend.
This is known to improve performance, at the cost of higher build times.
Consequently, you must also have `llvm` installed in order for `cabal new-build` to succeed.
To disable this locally, you can remove `-fllvm` from the `ghc-options`.

## Supported OpenQASM 3 Programs

PECAC currently supports the following features in OpenQASM 3.
- Qubit declarations.
- Input parameter declarations.
- All parameter-free gates in the standard library.
- All rotations in the standard library, taking affine rational linear combinations of parameter variables.
Note that all variable declarations must come before the first 

## Running PECAC

After building PECAC, you will find all executables under the following directory.
```
./dist-newstyle/build/[os_version]/[compiler_version]/[pecac_version]/x
```
This directory contains several testing utilities, along with the following tools.
- `pecac_reparam`: transforms circuits to have affine integer linear combinations of parameters.
- `pecac_exact`: exact equivalence checking for circuits with affine integer linear combinations of parameters.
- `pecac_prob`: probabilistic equivalence checking for circuits with affine integer linear combinations of parameters.
- `pecac_lphase`: linear phase estimation tool for verifying circuits up to linear global phase.
- `pecac_cutoffviewer`: computes cutoff bounds for quantifier elimination (helps to predict runtime).
- `pecac_cycloinst`: simulates a parameterized circuit on a specific choice of parameters using matrices of cyclotomic numbers.


### Phase Estimation

By default, PECAC will verify circuits exactly.
This means that `C1` and `C2` will be equivalent if and only if they are the same operator for all choices of parameters.
If `C1` and `C2` satisfy this equivalence relation, then they can be substituted in all contexts, including when controlled.
The tools `pecac_reparam` and `pecac_exact` accept a parameter `-g` which will compare `C1` and `C2` up to constant global phase.
In this mode, `C1` and `C2` are equivalent if they differ by a constant unitary multiplier.
A weaker notion of equivalence is equivalence up to affine linear global phase.
This means that the global phase can be written as a function`exp(a_1 * theta_1 + ... + a_k * theta_k)` where `theta_j` is the `j-th` parameter.

