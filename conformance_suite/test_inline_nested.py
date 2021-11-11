# test_inline_nested.py
# This should pass.
# This should terminate.
# This should be optimized.

from __static__ import inline
@inline
def e(x, y):
    return x + y
@inline
def f(x, y):
    return e(x, 3)
def g():
    return f(1,2)
# def test_inline_nested(self):
#     codestr = """
#         from __static__ import inline
#         @inline
#         def e(x, y):
#             return x + y
#         @inline
#         def f(x, y):
#             return e(x, 3)
#         def g():
#             return f(1,2)
#     """
#     with self.in_module(codestr, optimize=2) as mod:
#         g = mod.g
#         self.assertInBytecode(g, "LOAD_CONST", 4)
#         self.assertEqual(g(), 4)
