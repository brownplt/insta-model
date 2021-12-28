# Reason: Test hitted a banned word box
def test_dynamic_chained_assign_2(self):
    codestr = """
        from __static__ import int16, box
        def testfunc():
            x: int16
            y = x = 42
            return box(y)
    """
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(test(), 42)
