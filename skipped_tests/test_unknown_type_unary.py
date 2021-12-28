# Reason: Can't be translated by any of the three translator
def test_unknown_type_unary(self):
    codestr = """
        def x(y):
            z = -y
    """
    f = self.find_code(self.compile(codestr, modname="foo"))
    self.assertInBytecode(f, "UNARY_NEGATIVE")
