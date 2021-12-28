# Reason: Test hitted a banned word _kw
def test_method_prologue_kwonly_no_annotation(self):
    codestr = """
    def f(*, x):
        return 42
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "CHECK_ARGS", ())
        f(x=42)
