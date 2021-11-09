# procedure_check_argument_type_statically.py
# This should fail.

def f(x: int):
    pass

f("foo")