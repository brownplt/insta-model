# class_variables_redeclare_in_subclass_same_type.py
# This should pass.
# This should terminate.

from typing import ClassVar

class C1:
    x: ClassVar[int]

class C2(C1):
    x: ClassVar[int]
