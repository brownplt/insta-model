# unannotated_class_variables_readable.py
# This should fail.

from typing import ClassVar

class C:
    x = 42

obj = C()
assert obj.x is 42
