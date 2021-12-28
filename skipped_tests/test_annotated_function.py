# Reason: Can't be translated by any of the three translator
def test_annotated_function(self):
    codestr = """
    class C:
        def f(self) -> int:
            return 1
    def x(c: C):
        x = c.f()
        x += c.f()
        return x
    """
    code = self.compile(codestr, modname="foo")
    x = self.find_code(code, "x")
    self.assertInBytecode(x, "INVOKE_METHOD", (("foo", "C", "f"), 0))
    with self.in_module(codestr) as mod:
        x, C = mod.x, mod.C
        c = C()
        self.assertEqual(x(c), 2)
