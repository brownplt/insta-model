conformance_test:
	rm -r ./conformance_suite
	mkdir ./conformance_suite
	cp ../Playground/conformance_suite/*.py ./conformance_suite/ && \
	python3 ./python_tests_to_redex_tests.py && \
	racket ./conformance_suite.rkt && \
	echo "Your model passed all tests!"