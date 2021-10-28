def test_typed_swap_2(self):
    codestr = """
        def test(a):
            x: int
            y: str
            x, y = a, 'abc'
    """
    f = self.find_code(self.compile(codestr, modname="foo"))
    self.assertInBytecode(f, "CAST", ("builtins", "int"))
    self.assertNotInBytecode(f, "CAST", ("builtins", "str"))
