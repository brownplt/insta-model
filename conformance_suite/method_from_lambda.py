# method_from_lambda.py
# This should pass.
# This should terminate.

class C:
    m = lambda self: 2


obj = C()
assert obj.m() is 2