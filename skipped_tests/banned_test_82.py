# Reason: Test hitted a banned word int64
def test_bad_unbox_2(self):
    codestr = """
    from __static__ import unbox, int64
    def f():
        x:int64 = 42
        unbox(x, y)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "unbox only accepts a single argument"
    ):
        self.compile(codestr)
