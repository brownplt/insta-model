# Reason: Test hitted a banned word cbool
def test_no_cbool_math(self):
    codestr = """
        from __static__ import cbool
        def f(x: cbool, y: cbool) -> cbool:
            return x + y
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "cbool is not a valid operand type for add"
    ):
        self.compile(codestr)
