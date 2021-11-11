# Reason: Format too complicated
def test_unknown_type_binary(self):
    codestr = """
        def x(a, b):
            z = a + b
    """
    f = self.find_code(self.compile(codestr, modname="foo"))
    self.assertInBytecode(f, "BINARY_ADD")
