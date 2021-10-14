# assign_declared_field_neg.py
# This should fail.

class B:
    x: str

class C(B):
    pass

def f(c: C):
    c.x = 42
