# Reason: Can't be translated by any of the three translator
def test_typed_swap_nested_2(self):
    codestr = """
        def test(a):
            x: int
            y: str
            z: str
            ((x, y), z) = (1, a), 'foo'
    """
    f = self.find_code(self.compile(codestr, modname="foo"))
    self.assertInBytecode(f, "CAST", ("builtins", "str"))
    self.assertNotInBytecode(f, "CAST", ("builtins", "int"))
