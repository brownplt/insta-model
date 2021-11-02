test:
	rm ./skipped_tests/*
	rm ./conformance_suite/test_*
	python3 ./scripts/import_SP_tests.py && \
	python3 ./scripts/python_tests_to_redex_tests.py && \
	racket ./test-desugar.rkt && \
	racket ./test-statics.rkt && \
	echo "All done!"

statistics:
	echo "in skipped_tests"
	ls -l ./skipped_tests | wc -l
	echo "in conformance_suite"
	ls -l ./conformance_suite | wc -l