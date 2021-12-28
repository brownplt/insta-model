# Reason: Test hitted a banned word int64
def test_mixed_compare(self):
    codestr = """
    from __static__ import ssize_t, box, unbox
    def f(a):
        x: ssize_t = 0
        while x != a:
            pass
    """
    with self.assertRaisesRegex(TypedSyntaxError, "can't compare int64 to dynamic"):
        self.compile(codestr)
