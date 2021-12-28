# Reason: Test hitted a banned word int64
def test_int_compare64_mixed_sign(self):
    codestr = """
        from __static__ import uint64, int64
        def testfunc(tst):
            x: uint64 = 0
            y: int64 = 1
            if tst:
                x = x + 1
                y = y + 2
            if x < y:
                return True
            return False
    """
    with self.assertRaises(TypedSyntaxError):
        self.compile(codestr)
