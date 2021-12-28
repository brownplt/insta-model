# Reason: Can't be translated by any of the three translator
def test_typed_swap_nested_3(self):
    codestr = """
        def test(a):
            x: int
            y: int
            z: str
            ((x, y), z) = (1, 2), a
    """
    f = self.find_code(self.compile(codestr, modname="foo"))
    self.assertInBytecode(f, "CAST", ("builtins", "str"))
    # Currently because the tuple gets turned into a constant this is less than
    # ideal:
    self.assertInBytecode(f, "CAST", ("builtins", "int"))
