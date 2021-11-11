# Reason: Format too complicated
def test_assign_num_to_dynamic(self):
    codestr = """
        def f():
            x: foo = 42
    """
    code = self.compile(codestr, modname="foo")
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
