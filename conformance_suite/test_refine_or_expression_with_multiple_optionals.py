# test_refine_or_expression_with_multiple_optionals.py
# This should pass.
# This should terminate.

from typing import Optional
def f(s1: Optional[str], s2: Optional[str]) -> str:
    return s1 or s2 or "hi"
# def test_refine_or_expression_with_multiple_optionals(self):
#     codestr = """
#     from typing import Optional
#     def f(s1: Optional[str], s2: Optional[str]) -> str:
#         return s1 or s2 or "hi"
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertEqual(f("A", None), "A")
#         self.assertEqual(f(None, "B"), "B")
#         self.assertEqual(f("A", "B"), "A")
#         self.assertEqual(f(None, None), "hi")
