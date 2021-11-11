# test_final_constant_folding_disabled_on_nonfinals.py
# This should pass.
# This should terminate.
# This should be optimized.

from typing import Final
X: str = "omg"
def f() -> str:
    return X[1]
# def test_final_constant_folding_disabled_on_nonfinals(self):
#     codestr = """
#     from typing import Final
#     X: str = "omg"
#     def f() -> str:
#         return X[1]
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "LOAD_CONST", "omg")
#         self.assertInBytecode(f, "LOAD_GLOBAL", "X")
#         self.assertEqual(f(), "m")
