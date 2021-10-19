# override_instance_field_with_imprecise_type.py
# This should fail.

from typing import Any

class C:
    x: str

class D(C):
    x: Any