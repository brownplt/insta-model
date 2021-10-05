conformance_test:
	python3 ./python_tests_to_redex_tests.py && \
	racket ./conformance_suite.rkt && \
	echo "Your model passed all tests!"