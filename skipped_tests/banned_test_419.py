# Reason: Test hitted a banned word stararg
def test_extremum_non_specialization_dstararg(self):
    codestr = """
        def f() -> None:
            k = {
                "default": 5
            }
            min(3, 4, **k)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "COMPARE_OP")
        self.assertNotInBytecode(f, "POP_JUMP_IF_FALSE")
