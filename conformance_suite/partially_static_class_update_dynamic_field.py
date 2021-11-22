# partially_static_class_update_dynamic_field.py
# This should pass.
# This should terminate.

def make_C1():
    class C:
        x: str
    return C

C1 = make_C1()

class C2(C1):
    y: int

c = C2()
c.abc = "foo"
assert c.abc == "foo"