# procedure_works.py
# This should pass.
# This should terminate.

def f(x: int, y):
    return y - x

assert f(2, 3) is 1
