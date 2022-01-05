# test_max.py
# This should pass.
# This should terminate.

def f(a: int, b: int) -> int:
    return max(a, b)
assert f(1, 3) == 3
assert f(3, 1) == 3

# def test_max(self):
#     codestr = """
#         def f(a: int, b: int) -> int:
#             return max(a, b)
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "COMPARE_OP", ">=")
#         self.assertInBytecode(f, "POP_JUMP_IF_FALSE")
#         self.assertEqual(f(1, 3), 3)
#         self.assertEqual(f(3, 1), 3)
