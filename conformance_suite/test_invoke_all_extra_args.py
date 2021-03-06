# test_invoke_all_extra_args.py
# This should pass.
# This should terminate.

def target(a, b, c, d, e, f, g):
    return a * 2 + b * 3 + c * 4 + d * 5 + e * 6 + f * 7 + g
def testfunc():
    return target(1,2,3,4,5,6,7)
def main(f):
    assert f() == 119

main(testfunc)
# def test_invoke_all_extra_args(self):
#     codestr = """
#         def target(a, b, c, d, e, f, g):
#             return a * 2 + b * 3 + c * 4 + d * 5 + e * 6 + f * 7 + g
#         def testfunc():
#             return target(1,2,3,4,5,6,7)
#     """
#     with self.in_strict_module(codestr) as mod:
#         f = mod.testfunc
#         self.assertInBytecode(
#             f,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "target"), 7),
#         )
#         self.assertEqual(f(), 119)
