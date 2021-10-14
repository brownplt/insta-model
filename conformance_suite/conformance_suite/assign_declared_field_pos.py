# assign_declared_field_pos.py
# This should pass.

class B:
    x: int

class C(B):
    pass

def f(c: C):
    c.x = 42
