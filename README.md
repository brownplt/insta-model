# insta-model

The goal of this project is to model Static Python, a gradually typed variant
of Python developed at Instagram. To show that this project models Static
Python faithfully, we borrow hundreds of conformance tests from [Static
Python's test
suite](https://github.com/facebookincubator/cinder/blob/cinder/3.8/Lib/test/test_compiler/test_static/tests.py).
This repository includes a copy of the test suite (`tests.py`) fetched on
2022-01-09 (commit hash: `6d61575`).

## Prerequisites

- [Racket 8.3](https://download.racket-lang.org/racket-v8.3.html) (or newer).
  * Install Racket from the link above.
  * Make sure that the `racket` and `raco` executables are on your path.
- [Python 3.9](https://www.python.org/downloads/release/python-390/)
  * Make sure that a `python3.9` executable is on your path.
  * Newer versions of Python may work, but you'll need to change the `Makefile`
    to use the correct executable.

### Alternative: Docker

The `Dockerfile` declares an image with Racket and Python.
To use it, start the docker daemon (maybe with `sudo dockerd`) and run the
following commands:

```
docker build -t insta-model .
docker run -v "$PWD:/vol" -w /vol -ti insta-model bash
```

Windows users: replace `$PWD` with the absolute path to the `insta-model/` repo.


## How to run

Run `make`. That's all!

The `Makefile` builds and executes a comprehensive test suite for the model.
The main steps are:

 1. Import test cases from Static Python's test suite.
 2. Print statistics about the imported tests.
 3. Convert test cases to Redex syntax.
 4. Test the model's syntax, type checker, runtime, and correctness theorems.


#### Click Below for Example Output

<details><summary>Collected on 2022-04-07. Took 10 minutes to finish on a
laptop.</summary>

<pre>
echo "Importing tests from Static Python's test suite."
Importing tests from Static Python's test suite.
rm -f ./skipped_tests/*
rm -f ./conformance_suite/test_*
python3.9 ./scripts/import_SP_tests.py
bash scripts/stat.sh
-      537 skipped tests (from the .csv)
-      265 used tests (counting all ./conformance_suite/*test_*)
- 802 actual total (adding the previous two numbers)
-      802 expected total (counting all 'def test_' in tests.py)
echo "Translating Python files to tests of the model."
Translating Python files to tests of the model.
python3.9 ./scripts/python_tests_to_redex_tests.py > /dev/null
echo "Testing the model." && \
	echo "Testing the grammar." && \
	racket ./test-grammar.rkt && \
	echo "Testing the desugaring process." && \
	racket ./test-desugar.rkt && \
	echo "Testing the compiler and the type-checker." && \
	racket ./test-compile.rkt && \
	echo "Testing the runtime. This may take several minutes." && \
	racket ./test-runtime.rkt && \
	echo "Testing the soundness property with random programs. This will take even longer." && \
	racket ./conjectures.rkt
Testing the model.
Testing the grammar.
Testing the desugaring process.
Testing the compiler and the type-checker.
Testing the runtime. This may take several minutes.
Testing the soundness property with random programs. This will take even longer.
redex-check: /Users/ben/code/postdoc/kc/insta/insta-model/conjectures.rkt:172
no counterexamples in 10000 attempts
found 1742 well-typed expressions.
1343 of them reduce to a value of the expected type.
399 of them reduce to an error.
0 of them don't reduce to a value within the step limit.
redex-check: /Users/ben/code/postdoc/kc/insta/insta-model/conjectures.rkt:239
no counterexamples in 15000 attempts
found 1725 well-typed programs.
1701 of them terminate.
24 of them don't terminate within the step limit.
</pre>

_Note:_ the number of well-typed terms that reduce to a value may change
slightly across runs.
We conjecture that the variation is due to Redex's algorithm for generating
terms (despite the fact that
[we seed the Racket RNG](https://github.com/brownplt/insta-model/blob/main/conjectures.rkt#L8)
beforehand). We are looking into it!

</details>

## How to validate claims from the paper?

This section lists claims that we made in the paper, and where in this
repository to find the corresponding evidence.

> (page 10; Section 4) The model covers a substantial part of the Python
> language including assertions, loops, exception handlers, and delete
> statements.

These language constructs (and many others) are tested in the
[./conformance_suite](./conformance_suite). You can use `grep` to validate. For
example, to see whether we tested assertions, you can run:

```bash
$ grep 'assert ' -r ./conformance_suite/
```

To further validate that the conformance tests are indeeded tested, you can
edit the source code of a test to introduce a failure, re-run `make`, and check
that the test does fail.

> (page 10; Section 4) Second, we used Redex’s random testing tools to check
> type soundness on thousands of examples (1,600 expressions and 11,000
> programs).

We have two uses of
[`redex-check`](https://docs.racket-lang.org/redex/reference.html#%28form._%28%28lib._redex%2Freduction-semantics..rkt%29._redex-check%29%29)
in [./conjectures.rkt](./conjectures.rkt): line
[172](https://github.com/brownplt/insta-model/blob/main/conjectures.rkt#L172)
and line
[239](https://github.com/brownplt/insta-model/blob/main/conjectures.rkt#L239).

We will walk you through the first use. The other is very similar. On line 172
you will see the following expression. The expression generate 10000 random
expressions (`e+`) and checks whether for all expression `e+`, if the
expression compiles and terminates within certain number of reduction steps,
the result must be of the same type as the expression. The nonterminal `e+` is
defined in language `SP-conjecture` ([line 10 of
`conjectures.rkt`](https://github.com/brownplt/insta-model/blob/main/conjectures.rkt#L10)),
which inherite the definition from language `SP` ([line 29 of
`grammar.rkt`](https://github.com/brownplt/insta-model/blob/main/grammar.rkt#L29))

```racket
(redex-check SP-conjecture
             e+
             (term (compile-implies-terminate-implies-well-typed-e e+))
             #:attempts 10000)
```

In the repository we test 10000 random expressions and 15000 random programs.
In the paper, we report the numbers of **well-typed** terms that came from one
run: 1600 expressions and 11000 programs. You should see similar numbers of
well-typed terms if you re-run the experiment.

> (page 10; Section 4) we translated 265 tests from the Static Python
> regression suite to the syntax of the model and confirmed that the results do
> match, which suggests that the model conforms to actual Static Python.

You can see the number (265) if you run 

```bash
make statistics
```

> (page 10; Section 4) For most of the 265 tests, the translation is automatic.
> A few tests required hand- pruning to remove features that the model does not
> handle (52 total)

You can see the number (52) with 

```bash
ls ./conformance_suite/edited_test_* | wc -l
```

> (page 10; Section 4) Static Python has 537 other tests (802 total) that we
> did not use because they fall outside the scope of the model.

`make statistics` also produces this number (802).

> (page 13; Section 4.2.1) Most [casts] call for a tag check, i.e., a Python
> isinstance test. The sole exception is optional types, which require a tag
> check and a test for the none value.

If you look at [the definition of `compile-check` in
`./compile.rkt`](https://github.com/brownplt/insta-model/blob/main/compile.rkt#L1225),
the `dynamic` case is trivial. The function case is unreachable because there
is no function type in the surface syntax (neither for us nor for Static
Python, though we both have an internal representation for function types). The
remaining two cases are more interesting. When the target type is a class,
`compile-check` calls
[`check-exactness`](https://github.com/brownplt/insta-model/blob/main/compile.rkt#L1244),
which effectively performs a tag-check. The remaining case is for
`Optional[T]`, which has a test for the none value and a tag check for the
class `T`.

> (page 13; Section 4.2.1) No cast requires traversing a data structure.
> Similarly, no cast allocates a wrapper to check higher-order behaviors in a
> delayed fashion.

`compile-check` calls
[`check-exactness`](https://github.com/brownplt/insta-model/blob/main/compile.rkt#L1244),
which uses `is` or `issubclass` to compare the object's tag and the expected
tag. The object fields are never traversed.

## Conformance Tests

Each file under `conformance_suite` is a test case written as a commented
Python file. They come from two sources:

  1. Files named as `test_*` are extracted from `tests.py` with a script
     `scripts/import_SP_tests.py`. Files named as `edited_test_*` are edited
     after extraction. Some tests in `tests.py` are not extracted for various
     reasons. You can find in `left-out_reason.csv` reasons why each of them
     was left out. For each left-out reason, you can find the corresponding
     string patterns at the beginning of `scripts/import_SP_tests.py`.

  2. All other tests are written by us. `Statics.md` and `Dynamics.md` lists
     those tests together with their purposes.

## The Model

The script `scripts/python_tests_to_redex_tests.py` parses files in
`conformance_suite` and translate them to model tests. We have four test files:

- `test-grammar.rkt`
- `test-desugar.rkt`
- `test-compile.rkt`
- `test-runtime.rkt`

Each of the first three files contains all (ASTs of) Static Python programs in
`conformance_suite`. Some programs also occur in `test-runtime.rkt`.

The file `grammar.rkt` defines the grammar of the AST of the modeled subset of
Static Python.

The file `desugar.rkt` removes some syntactic sugars and performs some other
syntatic transformations that don't involve types. This file also defines the
output language, `SP-core`.

The file `compile.rkt` models the Static Python compiler and type-checker. The
most interesting definitions are `compile-e`, which compiles an expression, and
`compile-s`, which compiles an statement. Ill-typed programs should fail the
compilation.

The file `runtime.rkt` models the runtime semantics. The most interesting
definitions are `load` and `red-p`. The function `load` creates an execution
state out of a program. `red-p` is the reduction relation of execution states.

The file `conjectures.rkt` tests the model with randomly generated programs.

The file `utilities.rkt` provides helper definitions to all these model files.

## Other files

`Makefile` makes it convenient to run the model and to show the statistics of
test counts. If you want to dig deeper into this code base, it might be a good
idea to start from the `Makefile`.

`pj-7.1` directory contains the source code of our paper.

`run_conformance_suite.sh` is a script that can run conformance tests **using**
Static Python. This script has three major prerequisites:

 1. Static Python is installed. Follow the instructions in its
    [README](https://github.com/facebookincubator/cinder) to get started.
 2. The script must be copied to a directory `SP/<dir>/`, where `SP` is the
    name of the difertory where Static Python is installed and `<dir>` is the
    (arbitrary) name of a new subdirectory.
 3. There is a copy of `conformance_suite` inside `SP/<dir>/`

