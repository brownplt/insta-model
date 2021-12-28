# Reason: Test hitted a banned word float
def test_double_compare_with_literal(self):
    codestr = f"""
    from __static__ import double
    def testfunc(x: float) -> bool:
        y = double(x)
        if y > 3.14:
            return True
        return False
    """
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        self.assertTrue(f(4.1))
        self.assertFalse(f(1.1))
