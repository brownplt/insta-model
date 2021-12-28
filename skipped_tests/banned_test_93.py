# Reason: Test hitted a banned word int64
def test_error_primitive_cast(self):
    code = """
        from __static__ import int64, cast
        def f(a):
            x: int64 = 0
            return cast(x, int)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Call argument cannot be a primitive"
    ):
        self.compile(code)
