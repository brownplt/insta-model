# test_incompat_override_method_starargs.py
# This should fail.

class A:
    def m(self) -> int:
        return 42
class B(A):
    def m(self, *args) -> int:
        return 0