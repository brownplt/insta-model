#!/bin/bash

SKIPPED_TESTS=`cat left-out_reason.csv | tail -n +2 | wc -l`
USED_TESTS=$(ls -l ./conformance_suite/*test_* | wc -l)
ACTUAL_TOTAL=$(expr ${SKIPPED_TESTS} + ${USED_TESTS})
EXPECTED_TOTAL=$(grep 'def test_' tests.py | wc -l);
echo "- ${SKIPPED_TESTS} skipped tests (from the .csv)" ;
echo "- ${USED_TESTS} used tests (counting all ./conformance_suite/*test_*)" ;
echo "- ${ACTUAL_TOTAL} actual total (adding the previous two numbers)" ;
echo "- ${EXPECTED_TOTAL} expected total (counting all 'def test_' in tests.py)" ;