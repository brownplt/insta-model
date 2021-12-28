# Reason: Test hitted a banned word _kw
def test_extremum_non_specialization_kwarg(self):
    codestr = """
        def f() -> None:
            a = "4"
            b = "5"
            min(a, b, key=int)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "COMPARE_OP")
        self.assertNotInBytecode(f, "POP_JUMP_IF_FALSE")
