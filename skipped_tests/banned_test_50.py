# Reason: Test hitted a banned word double
def test_double_mixed_compare_reverse(self):
    codestr = """
    from __static__ import double, box, unbox
    def f(a):
        x: double = 0
        while a > x:
            pass
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "can't compare double to dynamic"
    ):
        self.compile(codestr)
