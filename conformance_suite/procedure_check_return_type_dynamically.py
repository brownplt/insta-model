# procedure_check_return_type_dynamically.py
# This should pass.
# This should error.

def asDyn(x):
    return x

def f() -> str:
    return asDyn(2)

f()
