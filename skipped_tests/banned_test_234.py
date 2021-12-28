# Reason: Test hitted a banned word _kw
def test_method_prologue_kwonly(self):
    codestr = """
    def f(*, x: str):
        return 42
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "CHECK_ARGS", (0, ("builtins", "str")))
        with self.assertRaisesRegex(
            TypeError, "f expected 'str' for argument x, got 'int'"
        ):
            f(x=42)
