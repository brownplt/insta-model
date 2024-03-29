everything: import_tests statistics redex_tests check_model

import_tests:
	echo "Importing tests from Static Python's test suite."
	rm -f ./skipped_tests/*
	rm -f ./conformance_suite/test_*
	python3.9 ./scripts/import_SP_tests.py

statistics:
	bash scripts/stat.sh

redex_tests:
	echo "Translating Python files to tests of the model."
	python3.9 ./scripts/python_tests_to_redex_tests.py > /dev/null

compile_model:
	raco make -v test-*.rkt

check_model: compile_model
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
