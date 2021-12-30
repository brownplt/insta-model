# method_override_inexact.py
# This should pass.
# This should terminate.

class C1:
    def m(self):
        return 2

class C2(C1):
    def m(self):
        return 3

def f() -> C1:
    return C2()
o = f()
assert o.m() is 3
