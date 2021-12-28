# Reason: Test hitted a banned word _kw
def test_generic_kwargs_method_unsupported(self):
    # definition is allowed, we just don't do an optimal invoke
    codestr = """
    class C:
        def f(self, a: int, b: str, **my_stuff) -> None:
            pass
    def g():
        return C().f(1, 'abc', x="y")
    """
    with self.in_module(codestr) as mod:
        g = mod.g
        self.assertInBytecode(g, "CALL_FUNCTION_KW", 3)
