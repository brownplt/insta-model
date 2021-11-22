# slots_contains_object_fields_but_not_class_fields.py
# This should pass.
# This should terminate.

from typing import ClassVar


class C:
    x: ClassVar[int] = 2
    y: str


assert C.__slots__ == ("y",)
