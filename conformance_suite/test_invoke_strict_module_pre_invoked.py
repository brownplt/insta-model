# test_invoke_strict_module_pre_invoked.py
# This should pass.

# This should be optimized.

def f():
    return 42
def g():
    return f()
# def test_invoke_strict_module_pre_invoked(self):
#     codestr = """
#         def f():
#             return 42
#         def g():
#             return f()
#     """
#     with self.in_strict_module(codestr) as mod:
#         self.assertEqual(mod.f(), 42)
#         self.assert_jitted(mod.f)
#         g = mod.g
#         self.assertEqual(g(), 42)
#         self.assertInBytecode(
#             g,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "f"), 0),
#         )
