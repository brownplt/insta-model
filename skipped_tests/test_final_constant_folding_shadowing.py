# Reason: Test hitted some skipped words
def test_final_constant_folding_shadowing(self):
    codestr = """
    from typing import Final
    X: Final[str] = "omg"
    def f() -> str:
        X = "lol"
        return X[1]
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "LOAD_CONST", "lol")
        self.assertNotInBytecode(f, "LOAD_GLOBAL", "omg")
        self.assertEqual(f(), "o")
