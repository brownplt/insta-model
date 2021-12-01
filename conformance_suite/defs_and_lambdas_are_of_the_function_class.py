# defs_and_lambdas_are_of_the_function_class.py
# This should pass.
# This should terminate.

def f(): return 2
assert type(f) is type(lambda: 3)