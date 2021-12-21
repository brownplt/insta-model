# class_variables_shadow_same_type_pos.py
# This should pass.
# This should terminate.

from typing import ClassVar

class C1:
    x: ClassVar[int]

class C2(C1):
    x: ClassVar[int]
