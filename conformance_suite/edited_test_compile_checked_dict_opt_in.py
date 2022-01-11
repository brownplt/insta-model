# test_compile_checked_dict_opt_in.py
# This should pass.
# This should terminate.

# We edited the next line because we don't support this convenient flag.
# from __static__.compiler_flags import checked_dicts
from __static__ import CheckedDict

class B: pass
class D(B): pass
def testfunc():
    # We edited the next line such that CheckedDict is used explicitly
    # x = {B():42, D():42}
    x = CheckedDict[B,int]({B():42, D():42})
    return x

# We edited the next line because we don't support first-class classes.
# def main(test, B):
def main(test):
    assert type(test()) == CheckedDict[B, int]

# We edited the next line because we don't support first-class classes.
# main(testfunc, B)
main(testfunc)

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
