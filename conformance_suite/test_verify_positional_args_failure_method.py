# test_verify_positional_args_failure_method.py
# This should fail.

class C:
    def x(self, a: int, b: str) -> None:
        pass
C().x("a", 2)