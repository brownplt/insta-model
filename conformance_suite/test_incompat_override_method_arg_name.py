# test_incompat_override_method_arg_name.py
# This should fail.

class A:
    def m(self, x: str) -> int:
        return 42
class B(A):
    def m(self, y: str) -> int:
        return 0