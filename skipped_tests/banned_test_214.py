# Reason: Test hitted a banned word int64
def test_int_swap(self):
    codestr = """
        from __static__ import int64, box
        def test():
            x: int64 = 42
            y: int64 = 100
            x, y = y, x
            return box(x), box(y)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("int64", "dynamic")
    ):
        self.compile(codestr, modname="foo")
