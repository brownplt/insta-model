# lookup_undeclared_field.py
# This should pass.

class C:
    pass

def expectInt(i: int):
    pass

def f(c: C):
    return expectInt(c.x)
