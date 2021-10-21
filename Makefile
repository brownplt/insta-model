conformance_test:
	rm ./conformance_suite/test_*
	python3 ./SP_tests_to_our_python_tests.py && \
	python3 ./python_tests_to_redex_tests.py && \
	racket ./test-statics.rkt && \
	echo "All done!"