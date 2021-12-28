# Reason: Test hitted a banned word float
def test_double_compare_with_integer_literal(self):
    codestr = f"""
    from __static__ import double
    def testfunc(x: float) -> bool:
        y = double(x)
        if y > 3:
            return True
        return False
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, re.escape("can't compare double to Literal[3]")
    ):
        self.compile(codestr)
