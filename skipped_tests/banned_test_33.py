# Reason: Test hitted a banned word int64
def test_int_add_mixed_64(self):
    codestr = """
        from __static__ import uint64, int64, box
        def testfunc(tst):
            x: uint64 = 0
            y: int64 = 1
            if tst:
                x = x + 1
                y = y + 2
            return box(x + y)
    """
    with self.assertRaisesRegex(TypedSyntaxError, "cannot add uint64 and int64"):
        self.compile(codestr)
