# test_with_traceback.py
# This should pass.
# This should terminate.
# This should be optimized.

def f():
    x = Exception()
    return x.with_traceback(None)
# def test_with_traceback(self):
#     codestr = """
#         def f():
#             x = Exception()
#             return x.with_traceback(None)
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertEqual(type(f()), Exception)
#         self.assertInBytecode(
#             f, "INVOKE_METHOD", (("builtins", "BaseException", "with_traceback"), 1)
#         )
