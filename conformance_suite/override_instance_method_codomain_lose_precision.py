# override_instance_method_codomain_lose_precision.py
# This should fail.

from typing import Any

class C1:
    def m(self) -> int:
        return 2

class C2(C1):
    def m(self) -> Any:
        return 3
