# test_invoke_all_reg_args.py
# This should pass.
# This should terminate.

def target(a, b, c, d, e, f):
    return a * 2 + b * 3 + c * 4 + d * 5 + e * 6 + f * 7
def testfunc():
    return target(1,2,3,4,5,6)
# def test_invoke_all_reg_args(self):
#     codestr = """
#         def target(a, b, c, d, e, f):
#             return a * 2 + b * 3 + c * 4 + d * 5 + e * 6 + f * 7
#         def testfunc():
#             return target(1,2,3,4,5,6)
#     """
#     with self.in_strict_module(codestr) as mod:
#         f = mod.testfunc
#         self.assertInBytecode(
#             f,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "target"), 6),
#         )
#         self.assertEqual(f(), 112)
