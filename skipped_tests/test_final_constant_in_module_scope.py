# Reason: Test hitted some skipped words
def test_final_constant_in_module_scope(self):
    codestr = """
    from typing import Final
    X: Final[int] = 21
    """
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.__final_constants__, ("X",))
