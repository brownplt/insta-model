# the_function_class_is_not_subclassable.py
# This should pass.
# This should error.

def f(): return 2

class C(type(f)): pass