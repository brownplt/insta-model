# Reason: Test hitted a banned word f"
def test_aug_add(self):
    codestr = """
    class C:
        def __init__(self):
            self.x = 1
    def f(a: C):
        a.x += 1
    """
    code = self.compile(codestr, modname="foo")
    code = self.find_code(code, name="f")
    self.assertInBytecode(code, "LOAD_FIELD", ("foo", "C", "x"))
    self.assertInBytecode(code, "STORE_FIELD", ("foo", "C", "x"))
