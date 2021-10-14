conformance_test:
	cp -r ../Playground/conformance_suite/ ./conformance_suite/ && \
	python3 ./python_tests_to_redex_tests.py && \
	racket ./conformance_suite.rkt && \
	echo "Your model passed all tests!"