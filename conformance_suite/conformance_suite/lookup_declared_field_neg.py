# lookup_declared_field_neg.py
# This should fail.

class C:
    x: str

def expectInt(i: int):
    pass

def f(c: C):
    return expectInt(c.x)
