# downcast_float_to_int_pos.py
# This should pass.
# This should terminate.

def asDyn(x):
    return x

x: float = 2
y: int = asDyn(x)