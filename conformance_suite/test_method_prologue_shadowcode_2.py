# test_method_prologue_shadowcode_2.py
# This should pass.
# This is an optimization test.
# This should terminate.

def f(x: str):
    return 42
# def test_method_prologue_shadowcode_2(self):
#     codestr = """
#     def f(x: str):
#         return 42
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "CHECK_ARGS", (0, ("builtins", "str")))
#         for i in range(100):
#             self.assertEqual(f("abc"), 42)
#         with self.assertRaisesRegex(
#             TypeError, ".*expected 'str' for argument x, got 'int'"
#         ):
#             f(42)
