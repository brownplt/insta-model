# test_assign_subtype_handling.py
# This should pass.

class B: pass
class D(B): pass
def f():
    b: B = B()
    b = D()
    b = B()