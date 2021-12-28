# Reason: Test hitted a banned word int32
def test_unwind(self):
    codestr = f"""
        from __static__ import int32
        def raises():
            raise IndexError()
        def testfunc():
            x: int32 = 1
            raises()
            print(x)
        """
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        with self.assertRaises(IndexError):
            f()
