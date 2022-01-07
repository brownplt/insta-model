# test_method_prologue_shadowcode.py
# This should pass.
# This should terminate.

def f(x, y: str):
    return 42
for i in range(100):
    assert f('abc', 'abc') == 42
try:
    f('abc', 42)
except TypeError:
    pass
else:
    raise Exception()

# def test_method_prologue_shadowcode(self):
#     codestr = """
#     def f(x, y: str):
#         return 42
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "CHECK_ARGS", (1, ("builtins", "str")))
#         for i in range(100):
#             self.assertEqual(f("abc", "abc"), 42)
#         with self.assertRaisesRegex(
#             TypeError, ".*expected 'str' for argument y, got 'int'"
#         ):
#             f("abc", 42)
