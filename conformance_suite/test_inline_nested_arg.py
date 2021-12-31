# test_inline_nested_arg.py
# This should pass.
# This is an optimization test.
# This should terminate.

from __static__ import inline
@inline
def e(x, y):
    return x + y
@inline
def f(x, y):
    return e(x, 3)
def g(a,b):
    return f(a,b)
# def test_inline_nested_arg(self):
#     codestr = """
#         from __static__ import inline
#         @inline
#         def e(x, y):
#             return x + y
#         @inline
#         def f(x, y):
#             return e(x, 3)
#         def g(a,b):
#             return f(a,b)
#     """
#     with self.in_module(codestr, optimize=2) as mod:
#         g = mod.g
#         self.assertInBytecode(g, "LOAD_CONST", 3)
#         self.assertInBytecode(g, "BINARY_ADD")
#         self.assertEqual(g(1, 2), 4)
