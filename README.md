# insta-model

`tests.py` is fetched on Jan 9, 2022. The commit hash ends with `6d61575`.

## The Model

The script `./scripts/python_tests_to_redex_tests.py` parses files written in
the surface language and translate them to tests. We have four kinds of tests:

- `./test-grammar.rkt`
- `./test-desugar.rkt`
- `./test-compile.rkt`
- `./test-runtime.rkt`

Each of the first three files contains all (ASTs of) Static Python programs in
`./conformance_suite`. Some programs also occur in `test-runtime.rkt`.

The file `./grammar.rkt` defines the grammar of the AST of the modeled subset of Static Python.

The file `./desugar.rkt` removes some syntactic sugars and performs some other syntatic transformations that don't involve types. This file also defined the output language, `SP-core`.

The file `./compile.rkt` models the Static Python compiler and type-checker. The most interesting definitions are `compile-e`, which compiles an expression, and `compile-s`, which compiles an statement. Ill-typed programs should fail the compilation.

The file `./runtime.rkt` models the runtime semantics. The most interesting definitions are `load` and `red-p`. The function `load` creates an execution state out of a program. `red-p` is the reduction relation of execution states.
