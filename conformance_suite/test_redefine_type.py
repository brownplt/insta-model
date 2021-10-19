# test_redefine_type.py
# This should fail.

class C: pass
class D: pass
def f(a):
    x: C = C()
    x: D = D()