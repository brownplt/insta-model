refresh_conformance_suite:
	rm -f ./skipped_tests/*
	rm -f ./conformance_suite/test_*
	python3 ./scripts/import_SP_tests.py && \
	python3 ./scripts/python_tests_to_redex_tests.py && \
	echo "All done!"

run_redex_tests:
	racket ./test-desugar.rkt && \
	racket ./test-statics.rkt && \
	racket ./test-compile.rkt && \
	racket ./test-dynamics.rkt && \
	echo "All done!"

statistics:
	echo "in skipped_tests"
	ls -l ./skipped_tests | wc -l
	echo "in conformance_suite"
	ls -l ./conformance_suite | wc -l