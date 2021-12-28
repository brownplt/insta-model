# Reason: Test hitted a banned word int64
def test_unbox_long(self):
    codestr = """
    from __static__ import unbox, int64
    def f():
        x:int64 = unbox(1)
    """
    self.compile(codestr)
