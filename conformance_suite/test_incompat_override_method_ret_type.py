# test_incompat_override_method_ret_type.py
# This should fail.

class A:
    def m(self) -> str:
        return "hello"
class B(A):
    def m(self) -> int:
        return 0