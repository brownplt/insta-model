# test_none_compare_reverse.py
# This should fail.

def f(x: int | None):
    if 1 > x:
        x = 1
    return x