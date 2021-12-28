# Reason: Test hitted a banned word box
def test_unbox_cbool_typed_unsupported(self):
    codestr = """
    from __static__ import cbool, box
    def f(i: int):
        x = cbool(i)
        return box(x)
    """
    self.type_error(codestr, "type mismatch: int cannot be assigned to cbool")
