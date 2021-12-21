# class_variables_may_shadow.py
# This should pass.
# This should terminate.

from typing import ClassVar

class C1:
    x: ClassVar[int]
C1.x = 2

class C2(C1):
    x: ClassVar[int]
C2.x = 3

assert C1.x is 2
assert C2.x is 3
