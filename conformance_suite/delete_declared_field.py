# delete_declared_field.py
# This should pass.

class C:
    x: str

def f(c: C):
    del c.x
