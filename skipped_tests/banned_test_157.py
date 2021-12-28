# Reason: Test hitted a banned word f"
def test_nonoptional_load(self):
    codestr = """
        class C:
            def __init__(self, y: int):
                self.y = y
        def f(c: C) -> int:
            return c.y
    """
    code = self.compile(codestr, modname="foo")
    f = self.find_code(code, "f")
    self.assertInBytecode(f, "LOAD_FIELD", ("foo", "C", "y"))
