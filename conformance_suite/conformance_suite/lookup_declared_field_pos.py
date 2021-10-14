# lookup_declared_field_pos.py
# This should pass.

class C:
    x: int

def expectInt(i: int):
    pass

def f(c: C):
    return expectInt(c.x)
