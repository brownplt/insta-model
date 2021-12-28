# Reason: Test hitted a banned word int64
def test_ifexp_verifies_type(self):
    codestr = """
    from __static__ import int64, clen, cbool
    def f(c):
        x: int64 = 1
        y: int64 = 2
        return [x if c else y]
    """
    self.type_error(codestr, "type mismatch: int64 cannot be assigned to dynamic")
