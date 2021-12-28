# test_invoke_strict_module.py
# This should pass.
# This should terminate.
# This should be optimized.

def f():
    return 42
def g():
    return f()
# def test_invoke_strict_module(self):
#     codestr = """
#         def f():
#             return 42
#         def g():
#             return f()
#     """
#     with self.in_strict_module(codestr) as mod:
#         g = mod.g
#         for i in range(100):
#             self.assertEqual(g(), 42)
#         self.assertInBytecode(g, "INVOKE_FUNCTION", ((mod.__name__, "f"), 0))
