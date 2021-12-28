# Reason: Test hitted a banned word int64
def test_error_nested_return_ann(self):
    code = """
        from __static__ import int64
        def f():
            x: int64 = 0
            def g() -> x:
                pass
            return g
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "return annotation cannot be a primitive"
    ):
        self.compile(code)
