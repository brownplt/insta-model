# override_instance_method_domain_lose_precision.py
# This should pass.

from typing import Any

class C1:
    def m(self, x: int):
        return 2

class C2(C1):
    def m(self, x: Any):
        return 3
