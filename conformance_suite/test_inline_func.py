# test_inline_func.py
# This should pass.

from __static__ import inline
@inline
def f(x, y):
    return x + y
def g():
    return f(1,2)
# def test_inline_func(self):
#     codestr = """
#         from __static__ import inline
#         @inline
#         def f(x, y):
#             return x + y
#         def g():
#             return f(1,2)
#     """
#     # we only inline at opt level 2 to avoid test patching problems
#     # TODO longer term we might need something better here (e.g. emit both
#     # inlined code and call and a guard to choose); assuming
#     # non-patchability at opt 2 works for IG but isn't generally valid
#     for enable_patching in [False, True]:
#         with self.subTest(enable_patching=enable_patching):
#             with self.in_module(codestr, enable_patching=enable_patching) as mod:
#                 g = mod.g
#                 if not enable_patching:
#                     self.assertInBytecode(g, "LOAD_CONST", 3)
#                 else:
#                     self.assertInBytecode(
#                         g, "INVOKE_FUNCTION", ((mod.__name__, "f"), 2)
#                     )
#                 self.assertEqual(g(), 3)
