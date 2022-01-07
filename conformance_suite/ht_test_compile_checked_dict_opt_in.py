# test_compile_checked_dict_opt_in.py
# This should pass.
# This should terminate.

# EDIT: We don't support this convenient flag.
# from __static__.compiler_flags import checked_dicts
from __static__ import CheckedDict

class B: pass
class D(B): pass
def testfunc():
    # EDIT: use CheckedDict explicitly
    # x = {B():42, D():42}
    x = CheckedDict[B,int]({B():42, D():42})
    return x
def main(test, B):
    assert type(test()) == CheckedDict[B, int]

main(testfunc, B)
# def test_compile_checked_dict_opt_in(self):
#     codestr = """
#         from __static__.compiler_flags import checked_dicts
#         class B: pass
#         class D(B): pass
#         def testfunc():
#             x = {B():42, D():42}
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         B = mod.B
#         self.assertEqual(type(test()), chkdict[B, int])
