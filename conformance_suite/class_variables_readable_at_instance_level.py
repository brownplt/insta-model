# class_variables_readable_at_instance_level.py
# This should pass.
# This should terminate.

from typing import ClassVar

class C:
    x: ClassVar[int]

C.x = 42
obj = C()
assert obj.x is 42
