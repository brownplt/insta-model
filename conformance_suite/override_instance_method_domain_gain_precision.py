# override_instance_method_domain_gain_precision.py
# This should fail.

from typing import Any

class C1:
    def m(self, x: Any):
        return 2

class C2(C1):
    def m(self, x: int):
        return 3
