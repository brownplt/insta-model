# override_instance_field_with_precise_type.py
# This should fail.

from typing import Any

class C:
    x: Any

class D(C):
    x: str