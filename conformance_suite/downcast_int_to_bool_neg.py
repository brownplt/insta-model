# downcast_int_to_bool_neg.py
# This should pass.
# This should error.


def asDyn(x):
    return x

x: int = 2
y: bool = asDyn(x)