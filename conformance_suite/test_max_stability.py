# test_max_stability.py
# This should pass.
# This should terminate.
# This should be optimized.

def f(a: int, b: int) -> int:
    return max(a, b)
# def test_max_stability(self):
#     codestr = """
#         def f(a: int, b: int) -> int:
#             return max(a, b)
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "COMPARE_OP", ">=")
#         self.assertInBytecode(f, "POP_JUMP_IF_FALSE")
#         # p & q should be different objects, but with same value
#         p = int("11334455667")
#         q = int("11334455667")
#         self.assertNotEqual(id(p), id(q))
#         # Since p and q are equal, the returned value should be the first arg
#         self.assertEqual(id(f(p, q)), id(p))
#         self.assertEqual(id(f(q, p)), id(q))
