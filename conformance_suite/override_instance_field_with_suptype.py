# override_instance_field_with_suptype.py
# This should fail.

class C:
    x: bool

class D(C):
    x: int