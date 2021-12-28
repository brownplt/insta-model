# Reason: Test hitted a banned word cbool
def test_or_verifies_type(self):
    codestr = """
    from __static__ import cbool
    def f():
        x: cbool = False
        y: cbool = True
        return [x or y]
    """
    self.type_error(codestr, "type mismatch: cbool cannot be assigned to dynamic")
