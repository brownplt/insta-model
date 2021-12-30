# downcast_C1_to_C2_neg.py
# This should pass.
# This should error.

class C1:
    pass

class C2(C1):
    pass

def asDyn(x):
    return x

x: C1 = C1()
y: C2 = asDyn(x)
