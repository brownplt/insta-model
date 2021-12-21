# class_variables_should_be_declared_with_ClassVar_pos.py
# This should pass.
# This should terminate.

from typing import ClassVar

class C:
    x: ClassVar[int] = 42

assert C.x is 42
