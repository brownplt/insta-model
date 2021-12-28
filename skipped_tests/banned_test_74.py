# Reason: Test hitted a banned word int64
def test_unbox_incompat_type(self):
    codestr = """
    from __static__ import int64, box
    def f(i: str):
        x:int64 = int64(i)
        return box(x)
    """
    with self.assertRaisesRegex(TypedSyntaxError, type_mismatch("str", "int64")):
        self.compile(codestr)
