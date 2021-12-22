# class_variables_shadow_by_instance_variables_same_class.py
# This should fail.

from typing import ClassVar

class C:
    x: ClassVar[int]
    x: int
