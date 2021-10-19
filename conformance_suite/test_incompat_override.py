# test_incompat_override.py
# This should fail.

class C:
    x: int
class D(C):
    def x(self): pass