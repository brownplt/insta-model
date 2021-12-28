# Reason: Can't be translated by any of the three translator
def test_typed_swap_member(self):
    codestr = """
        class C:
            def __init__(self):
                self.x: int = 42
        def test(a):
            x: int
            y: str
            C().x, y = a, 'abc'
    """
    f = self.find_code(self.compile(codestr, modname="foo"), "test")
    self.assertInBytecode(f, "CAST", ("builtins", "int"))
    self.assertNotInBytecode(f, "CAST", ("builtins", "str"))
