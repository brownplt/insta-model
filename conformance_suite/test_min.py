# test_min.py
# This should pass.
# This is an optimization test.
# This should terminate.

def f(a: int, b: int) -> int:
    return min(a, b)
# def test_min(self):
#     codestr = """
#         def f(a: int, b: int) -> int:
#             return min(a, b)
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "COMPARE_OP", "<=")
#         self.assertInBytecode(f, "POP_JUMP_IF_FALSE")
#         self.assertEqual(f(1, 3), 1)
#         self.assertEqual(f(3, 1), 1)
