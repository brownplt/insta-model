# unannotated_class_variables_redeclare.py
# This should fail.

from typing import Any, ClassVar

class C:
    x = 2
    x: ClassVar[Any]
