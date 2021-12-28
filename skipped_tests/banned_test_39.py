# Reason: Test hitted a banned word double
def test_double_unary_unsupported(self):
    codestr = f"""
    from __static__ import double, box
    def testfunc(tst):
        x: double = 1.0
        if tst:
            x = x + 1
        x = ~x
        return box(x)
    """
    with self.assertRaisesRegex(TypedSyntaxError, "Cannot invert/not a double"):
        self.compile(codestr)
