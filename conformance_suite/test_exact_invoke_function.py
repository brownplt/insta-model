# test_exact_invoke_function.py
# This should pass.
# This should terminate.
# This should be optimized.

def f() -> str:
    return ", ".join(['1','2','3'])
# def test_exact_invoke_function(self):
#     codestr = """
#         def f() -> str:
#             return ", ".join(['1','2','3'])
#     """
#     f = self.find_code(self.compile(codestr))
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(
#             f, "INVOKE_FUNCTION", (("builtins", "str", "join"), 2)
#         )
#         f()
