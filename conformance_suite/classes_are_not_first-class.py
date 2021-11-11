# classes_are_not_first-class.py
# This should pass.
# This should terminate.

class C:
    pass

def checkExpect(cls, obj):
    x: cls = obj
    return x

checkExpect(C, 42)