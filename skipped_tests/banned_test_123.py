# Reason: Test hitted a banned word int64
def test_error_nested_class_prim_kwarg(self):
    code = """
        from __static__ import int64, unbox
        from typing import Final
        X: Final[int] = 42
        class C(metaclass=int64(X)): pass
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "class kwarg cannot be a primitive"
    ):
        self.compile(code)
