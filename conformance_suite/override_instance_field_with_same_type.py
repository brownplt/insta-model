# override_instance_field_with_same_type.py
# This should pass.

class C:
    x: int

class D(C):
    x: int