# method_override_exact.py
# This should pass.
# This should terminate.

class C1:
    def m(self):
        return 2

class C2(C1):
    def m(self):
        return 3

o = C2()
assert o.m() is 3
