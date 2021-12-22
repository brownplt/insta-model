# class_variables_shadow_by_instance_variables_sub_class.py
# This should fail.

from typing import ClassVar

class C1:
    x: ClassVar[int]

class C2(C1):
    x: int
