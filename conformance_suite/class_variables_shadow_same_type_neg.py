# class_variables_shadow_same_type_neg.py
# This should fail.

from typing import ClassVar

class C1:
    x: ClassVar[int]

class C2(C1):
    x: ClassVar[str]
