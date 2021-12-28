# Reason: Test hitted a banned word int64
def test_error_primitive_after_starred(self):
    code = """
        from __static__ import int64
        def g(*args):
            pass
        def f(a):
            x: int64 = 0
            y = []
            return g(*y, x)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Call argument cannot be a primitive"
    ):
        self.compile(code)
