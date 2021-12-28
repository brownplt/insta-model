# Reason: Test hitted a banned word int64
def test_error_nested_ann(self):
    code = """
        from __static__ import int64
        def f():
            x: int64 = 0
            def g(foo: x):
                pass
            return g
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "argument annotation cannot be a primitive"
    ):
        self.compile(code)
