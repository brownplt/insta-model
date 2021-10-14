# lookup_parent_field_pos.py
# This should pass.

class B:
    x: int

class C(B):
    pass

def expectInt(i: int):
    pass

def f(c: C):
    return expectInt(c.x)
