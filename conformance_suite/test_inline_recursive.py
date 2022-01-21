# test_inline_recursive.py
# This should pass.

from __static__ import inline
@inline
def f(x, y):
    return f(x, y)
def g():
    return f(1,2)
# def test_inline_recursive(self):
#     codestr = """
#         from __static__ import inline
#         @inline
#         def f(x, y):
#             return f(x, y)
#         def g():
#             return f(1,2)
#     """
#     with self.in_module(codestr, optimize=2) as mod:
#         g = mod.g
#         self.assertInBytecode(g, "INVOKE_FUNCTION", (((mod.__name__, "f"), 2)))
