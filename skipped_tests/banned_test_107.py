# Reason: Test hitted a banned word int64
def test_lambda_ret_primitive(self):
    code = """
        from __static__ import int64
        from typing import Final
        X: Final[int] = 42
        def f():
            return lambda: int64(X)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "lambda cannot return primitive value"
    ):
        self.compile(code)
