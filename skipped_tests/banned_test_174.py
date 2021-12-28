# Reason: Test hitted a banned word vararg
def test_generic_varargs_method_unsupported(self):
    # definition is allowed, we just don't do an optimal invoke
    codestr = """
    class C:
        def f(self, a: int, b: str, *my_stuff) -> None:
            pass
    def g():
        return C().f(1, 'abc', "foo")
    """
    with self.in_module(codestr) as mod:
        g = mod.g
        self.assertInBytecode(g, "CALL_METHOD", 3)
