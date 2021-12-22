# class_variables_redeclare.py
# This should fail.

from typing import ClassVar

class C:
    x: ClassVar[int]
    x: ClassVar[int]
