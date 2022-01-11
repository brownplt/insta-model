everything: import python_test_to_redex_test

import:
	rm -f ./skipped_tests/*
	rm -f ./conformance_suite/test_*
	python3.9 ./scripts/import_SP_tests.py

python_test_to_redex_test:
	python3.9 ./scripts/python_tests_to_redex_tests.py

run_redex_tests:
	racket ./test-grammar.rkt && \
	racket ./test-desugar.rkt && \
	racket ./test-compile.rkt && \
	racket ./test-runtime.rkt

statistics:
	grep 'def test_' ./tests.py | wc -l
	ls -1 ./conformance_suite/test_* ./conformance_suite/ht_*  | wc -l
	ls -1 ./skipped_tests | wc -l
