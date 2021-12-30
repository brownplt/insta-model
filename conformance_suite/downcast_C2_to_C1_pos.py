# downcast_C1_to_C2_pos.py
# This should pass.
# This should terminate.

class C1:
    pass

class C2(C1):
    pass

def asDyn(x):
    return x

x: C1 = C2()
y: C2 = asDyn(x)
