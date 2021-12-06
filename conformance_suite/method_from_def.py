# method_from_def.py
# This should pass.
# This should terminate.

class C:
    def m(self):
        return 2


obj = C()
assert obj.m() is 2