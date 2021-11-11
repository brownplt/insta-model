# Reason: Format too complicated
def test_subclass_binop(self):
    codestr = """
        class C: pass
        class D(C): pass
        def f(x: C, y: D):
            return x + y
    """
    code = self.compile(codestr, modname="foo")
    f = self.find_code(code, "f")
    self.assertInBytecode(f, "BINARY_ADD")
