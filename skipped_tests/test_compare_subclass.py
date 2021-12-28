# Reason: Can't be translated by any of the three translator
def test_compare_subclass(self):
    codestr = """
    class C: pass
    class D(C): pass
    x = C() > D()
    """
    code = self.compile(codestr)
    self.assertInBytecode(code, "COMPARE_OP")
