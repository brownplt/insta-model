# override_instance_field.py
# This should fail.

class C:
    x: int

class D(C):
    x: int