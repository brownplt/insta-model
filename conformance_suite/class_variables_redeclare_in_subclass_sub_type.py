# class_variables_redeclare_in_subclass_sub_type.py
# This should fail.

from typing import ClassVar

class C1:
    x: ClassVar[int]

class C2(C1):
    x: ClassVar[bool]
