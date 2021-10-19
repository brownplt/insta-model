# test_redefine_local_type.py
# This should fail.

class C: pass
class D: pass
def f():
    x: C = C()
    x: D = D()