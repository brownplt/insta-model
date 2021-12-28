# Reason: Test hitted a banned word int64
def test_clen_verifies_type(self):
    codestr = """
    from __static__ import int64, clen
    def f():
        return [clen([1])]
    """
    self.type_error(codestr, "type mismatch: int64 cannot be assigned to dynamic")
