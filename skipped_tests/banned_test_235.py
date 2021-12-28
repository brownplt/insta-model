# Reason: Test hitted a banned word _kw
def test_method_prologue_kwonly_2(self):
    codestr = """
    def f(x, *, y: str):
        return 42
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "CHECK_ARGS", (1, ("builtins", "str")))
        with self.assertRaisesRegex(
            TypeError, "f expected 'str' for argument y, got 'object'"
        ):
            f(1, y=object())
