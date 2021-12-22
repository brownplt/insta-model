# class_variables_declare_only.py
# This should pass.
# This should terminate.

from typing import ClassVar

class C:
    x: ClassVar[int]