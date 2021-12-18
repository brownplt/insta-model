# methods_mutable.py
# This should pass.
# This should terminate.

class C:
    def m(self):
        return 2

C.m = lambda self: 3
