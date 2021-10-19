# override_instance_field_with_subtype.py
# This should fail.

class C:
    x: int

class D(C):
    x: bool