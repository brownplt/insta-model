# method_override_dynamic.py
# This should pass.
# This should terminate.

class C1:
    def m(self):
        return 2

class C2(C1):
    def m(self):
        return 3

def f():
    return C2()
o = f()
assert o.m() is 3
