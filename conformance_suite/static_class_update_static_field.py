# static_class_update_static_field.py
# This should pass.
# This should terminate.

class C1:
    x: str

class C2(C1):
    y: int

c = C2()
c.x = "foo"
assert c.x is "foo"