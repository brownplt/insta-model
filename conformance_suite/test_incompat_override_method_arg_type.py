# test_incompat_override_method_arg_type.py
# This should fail.

class A:
    def m(self, x: str) -> int:
        return 42
class B(A):
    def m(self, x: int) -> int:
        return 0