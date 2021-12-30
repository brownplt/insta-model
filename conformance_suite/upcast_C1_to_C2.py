# upcast_C1_to_C2
# This should pass.
# This should terminate.

class C1:
    pass

class C2(C1):
    pass

def f():
    return C2()

x: C1 = f()
