everything: import python_test_to_redex_test

import:
	rm -f ./skipped_tests/*
	rm -f ./conformance_suite/test_*
	python3.9 ./scripts/import_SP_tests.py

python_test_to_redex_test:
	python3.9 ./scripts/python_tests_to_redex_tests.py

run_redex_tests:
	racket ./test-desugar.rkt && \
	# racket ./test-statics.rkt && \
	racket ./test-compile.rkt && \
	racket ./test-dynamics.rkt && \
	echo "All done!"

statistics:
	echo "in skipped_tests"
	ls -l ./skipped_tests | wc -l
	echo "in conformance_suite"
	ls -l ./conformance_suite/test_* ./conformance_suite/ht_*  | wc -l