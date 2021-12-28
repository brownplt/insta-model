# Reason: Test hitted a banned word int8
def test_widening_assign(self):
    codestr = """
        from __static__ import int8, int16, box
        def testfunc():
            x: int16
            y: int8
            x = y = 42
            return box(x), box(y)
    """
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(test(), (42, 42))
