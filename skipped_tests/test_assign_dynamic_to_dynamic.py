# Reason: Can't be translated by any of the three translator
def test_assign_dynamic_to_dynamic(self):
    codestr = """
        def f(C):
            x: unknown = C()
    """
    code = self.compile(codestr, modname="foo")
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
