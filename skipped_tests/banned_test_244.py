# Reason: Test hitted a banned word int8
def test_narrowing_assign_literal(self):
    codestr = """
        from __static__ import int8, int16, box
        def testfunc():
            x: int8
            y: int16
            x = y = 42
            return box(x), box(y)
    """
    self.compile(codestr, modname="foo")
