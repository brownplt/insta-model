# Reason: Test hitted a banned word int64
def test_format_primitive(self):
    code = """
        from __static__ import int64
        def f():
            x: int64 = 0
            return f"{x}"
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "cannot use primitive in formatted value"
    ):
        self.compile(code)
