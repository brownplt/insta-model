# lookup_parent_field_neg.py
# This should fail.

class B:
    x: str

class C(B):
    pass

def expectInt(i: int):
    pass

def f(c: C):
    return expectInt(c.x)
