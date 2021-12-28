# Reason: Test hitted a banned word int64
def test_set_primitive(self):
    code = """
        from __static__ import int64
        def f():
            x: int64 = 1
            return {x}
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "set members cannot be primitives"
    ):
        self.compile(code)
