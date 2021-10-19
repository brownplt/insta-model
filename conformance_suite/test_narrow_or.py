# test_narrow_or.py
# This should pass.

def f(x: int | None) -> int:
    if x is None or x > 1:
        x = 1
    return x