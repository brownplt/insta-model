# unannotated_class_variables_readable.py
# This should pass.
# This should terminate.

class C:
    x = 42

obj = C()
assert obj.x is 42
