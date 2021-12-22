# unannotated_class_variables_redeclare_in_subclass_same_type.py
# This should pass.
# This should terminate.

from typing import ClassVar, Any

class C1:
    x = 2

class C2(C1):
    x: ClassVar[Any]
