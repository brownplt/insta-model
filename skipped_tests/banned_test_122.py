# Reason: Test hitted a banned word int64
def test_error_nested_class_prim_decorator(self):
    code = """
        from __static__ import int64, unbox
        from typing import Final
        X: Final[int] = 42
        @int64(X)
        class C: pass
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "decorator cannot be a primitive"
    ):
        self.compile(code)
