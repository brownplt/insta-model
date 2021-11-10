def test_final_constant_folding_str(self):
    codestr = """
    from typing import Final
    X: Final[str] = "omg"
    def f() -> str:
        return X[1]
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "LOAD_CONST", "omg")
        self.assertNotInBytecode(f, "LOAD_GLOBAL")
        self.assertEqual(f(), "m")
