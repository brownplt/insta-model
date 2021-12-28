# Reason: Test hitted a banned word int32
def test_cmpop(self):
    codestr = """
        from __static__ import int32
        def f():
            i: int32 = 0
            j: int = 0
            if i == 0:
                return 0
            if j == 0:
                return 1
    """
    code = self.compile(codestr, modname="foo")
    x = self.find_code(code, "f")
    self.assertInBytecode(x, "PRIMITIVE_COMPARE_OP", 0)
    self.assertInBytecode(x, "COMPARE_OP", "==")
