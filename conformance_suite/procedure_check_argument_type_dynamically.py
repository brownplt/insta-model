# procedure_check_argument_type_dynamically.py
# This should pass.
# This should error.

def asDyn(x):
    return x

def f(x: int):
    pass

f(asDyn("foo"))