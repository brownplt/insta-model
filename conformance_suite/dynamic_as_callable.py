# dynamic_as_callable.py
# This should pass.

def f(x):
    return x(x(2), x("foo"))
