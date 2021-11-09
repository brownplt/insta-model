# procedure_check_arity_dynamically.py
# This should pass.
# This should error.

def asDyn(x):
    return x

def f(x, y):
    pass

asDyn(f)(2)