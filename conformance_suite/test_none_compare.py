# test_none_compare.py
# This should fail.

def f(x: int | None):
    if x > 1:
        x = 1
    return x