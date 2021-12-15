# override_instance_method_codomain_gain_precision.py
# This should pass.

from typing import Any

class C1:
    def m(self) -> Any:
        return 2

class C2(C1):
    def m(self) -> int:
        return 3
