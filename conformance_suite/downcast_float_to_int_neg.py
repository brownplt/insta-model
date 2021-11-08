# downcast_float_to_int_neg.py
# This should pass.
# This should error.


def asDyn(x):
    return x

x: float = 2.3
y: int = asDyn(x)