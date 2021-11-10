def test_final_constant_folding_bool(self):
    codestr = """
    from typing import Final
    X: Final[bool] = True
    def f() -> bool:
        return not X
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "LOAD_CONST", True)
        self.assertNotInBytecode(f, "LOAD_GLOBAL")
        self.assertFalse(f())
