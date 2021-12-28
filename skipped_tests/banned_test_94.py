# Reason: Test hitted a banned word int64
def test_error_primitive_isinstance(self):
    code = """
        from __static__ import int64
        def f(a):
            x: int64 = 0
            return isinstance(x, int)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Call argument cannot be a primitive"
    ):
        self.compile(code)
