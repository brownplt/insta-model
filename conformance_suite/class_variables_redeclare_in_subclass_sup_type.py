# class_variables_redeclare_in_subclass_sup_type.py
# This should fail.

from typing import ClassVar

class C1:
    x: ClassVar[bool]

class C2(C1):
    x: ClassVar[int]
