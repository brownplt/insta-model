# Reason: Test hitted a banned word int32
def test_index_by_int(self):
    codestr = """
        from __static__ import int32
        def f(x):
            i: int32 = 0
            return x[i]
    """
    with self.assertRaises(TypedSyntaxError):
        self.compile(codestr)
