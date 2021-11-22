# static_class_update_dynamic_field.py
# This should pass.
# This should error.

class C1:
    x: str

class C2(C1):
    y: int

c = C2()
c.abc = "foo"