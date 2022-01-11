# insta-model

The goal of this project is to model Static Python, a gradually typed variant of Python developped at Instagram. To show that this project models Static Python faithfully, we borrow hundreds of conformance tests from [Static Python's test suite](https://github.com/facebookincubator/cinder/blob/cinder/3.8/Lib/test/test_compiler/test_static/tests.py). This repository includes a copy of the test suite (`tests.py`) fetched on Jan 9, 2022 (commit hash: `6d61575`).

## Conformance Tests

Each file under `/conformance_suite` is a test case written as a commented Python file. They come from three sources. Files named as `test_*` are extracted from `/tests/py` with a script `/scripts/import_SP_tests.py`. Files named as `edited_test_*` are edited after extraction. All other tests are written by us. `/Statics.md` and `/Dynamics.md` lists those tests together with their purposes.

## The Model

The script `/scripts/python_tests_to_redex_tests.py` parses files in `/conformance_suite` and translate them to model tests. We have four kinds of tests:

- `/test-grammar.rkt`
- `/test-desugar.rkt`
- `/test-compile.rkt`
- `/test-runtime.rkt`

Each of the first three files contains all (ASTs of) Static Python programs in
`/conformance_suite`. Some programs also occur in `test-runtime.rkt`.

The file `/grammar.rkt` defines the grammar of the AST of the modeled subset of Static Python.

The file `/desugar.rkt` removes some syntactic sugars and performs some other syntatic transformations that don't involve types. This file also defined the output language, `SP-core`.

The file `/compile.rkt` models the Static Python compiler and type-checker. The most interesting definitions are `compile-e`, which compiles an expression, and `compile-s`, which compiles an statement. Ill-typed programs should fail the compilation.

The file `/runtime.rkt` models the runtime semantics. The most interesting definitions are `load` and `red-p`. The function `load` creates an execution state out of a program. `red-p` is the reduction relation of execution states.
