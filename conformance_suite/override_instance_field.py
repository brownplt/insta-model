# override_instance_field_with_incompatible_type.py
# This should fail.

class C:
    x: int

class D(C):
    x: str