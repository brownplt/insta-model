# test_method_prologue_no_annotation.py
# This should pass.
# This is an optimization test.
# This should terminate.

def f(x):
    return 42
# def test_method_prologue_no_annotation(self):
#     codestr = """
#     def f(x):
#         return 42
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "CHECK_ARGS", ())
#         self.assertEqual(f("abc"), 42)
