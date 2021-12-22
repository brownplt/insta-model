# class_variables_redeclare_in_subclass_more_precise_type.py
# This should fail.

from typing import ClassVar, Any

class C1:
    x: ClassVar[Any]

class C2(C1):
    x: ClassVar[int]
