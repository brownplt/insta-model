# test_compile_checked_dict_opt_out_by_default.py
# This should pass.
# This should terminate.

class B: pass
class D(B): pass
def testfunc():
    x = {B():42, D():42}
    return x
def main(test, B):
    assert type(test()) == dict

main(testfunc, B)
# def test_compile_checked_dict_opt_out_by_default(self):
#     codestr = """
#         class B: pass
#         class D(B): pass
#         def testfunc():
#             x = {B():42, D():42}
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         B = mod.B
#         self.assertEqual(type(test()), dict)
