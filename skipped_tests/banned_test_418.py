# Reason: Test hitted a banned word stararg
def test_extremum_non_specialization_stararg(self):
    codestr = """
        def f() -> None:
            a = [3, 4]
            min(*a)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "COMPARE_OP")
        self.assertNotInBytecode(f, "POP_JUMP_IF_FALSE")
