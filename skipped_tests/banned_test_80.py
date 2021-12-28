# Reason: Test hitted a banned word int64
def test_bad_unbox(self):
    codestr = """
    from __static__ import unbox, int64
    def f():
        x:int64 = 42
        unbox(x)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Call argument cannot be a primitive"
    ):
        self.compile(codestr)
