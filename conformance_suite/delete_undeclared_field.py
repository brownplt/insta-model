# delete_undeclared_field.py
# This should pass.

class C:
    pass

def f(c: C):
    del c.x
