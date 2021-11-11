# test_refine_or_expression.py
# This should pass.
# This should terminate.


from typing import Optional
def f(s: Optional[str]) -> str:
    return s or "hi"
# def test_refine_or_expression(self):
#     codestr = """
#     from typing import Optional
#     def f(s: Optional[str]) -> str:
#         return s or "hi"
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertEqual(f("A"), "A")
#         self.assertEqual(f(None), "hi")
