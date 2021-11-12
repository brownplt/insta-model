# downcast_int_to_bool_pos.py
# This should pass.
# This should terminate.

def asDyn(x):
    return x

x: int = True
y: bool = asDyn(x)