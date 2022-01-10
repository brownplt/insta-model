# test_compile_checked_dict_with_annotation.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
class B: pass
def testfunc():
    x: CheckedDict[B, int] = {B():42}
    return x
# The next line is edited because we don't support first-class classes.
# def main(test, B):
def main(test):
    assert type(test()) == CheckedDict[B, int]

# The next line is edited because we don't support first-class classes.
# main(testfunc, B)
main(testfunc)
# def test_compile_checked_dict_with_annotation(self):
#     codestr = """
#         from __static__ import CheckedDict
#         class B: pass
#         def testfunc():
#             x: CheckedDict[B, int] = {B():42}
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         B = mod.B
#         self.assertEqual(type(test()), chkdict[B, int])
