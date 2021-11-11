# Reason: Test hitted some skipped words
def test_final_constant_folding_int(self):
    codestr = """
    from typing import Final
    X: Final[int] = 1337
    def plus_1337(i: int) -> int:
        return i + X
    """
    with self.in_module(codestr) as mod:
        plus_1337 = mod.plus_1337
        self.assertInBytecode(plus_1337, "LOAD_CONST", 1337)
        self.assertNotInBytecode(plus_1337, "LOAD_GLOBAL")
        self.assertEqual(plus_1337(3), 1340)
