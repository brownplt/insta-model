# class_variables_immutable_at_instance_level.py
# This should fail.

from typing import ClassVar

class C:
    x: ClassVar[int]

obj = C()
obj.x = 42
