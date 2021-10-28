def test_default_arg_non_const(self):
    codestr = """
    class C: pass
    def x(val=C()) -> C:
        return val
    def f() -> C:
        return x()
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "CALL_FUNCTION")
