# test_union_compare.py
# This should pass.
# This should terminate.


def f(x: int | float) -> bool:
    return x > 0
# def test_union_compare(self):
#     codestr = """
#         def f(x: int | float) -> bool:
#             return x > 0
#     """
#     with self.in_strict_module(codestr) as mod:
#         self.assertEqual(mod.f(3), True)
#         self.assertEqual(mod.f(3.1), True)
#         self.assertEqual(mod.f(-3), False)
#         self.assertEqual(mod.f(-3.1), False)
