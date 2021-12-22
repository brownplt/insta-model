# unannotated_class_variables_nonwritable.py
# This should fail.

from typing import Any

class C:
    x = 2

obj = C()
obj.x = 3
