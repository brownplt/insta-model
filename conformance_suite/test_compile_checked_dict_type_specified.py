# test_compile_checked_dict_type_specified.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
class B: pass
class D(B): pass
def testfunc():
    x: CheckedDict[B, int] = CheckedDict[B, int]({D():42})
    return x
def main(test, B):
    assert type(test()) == CheckedDict[B, int]

main(testfunc, B)
# def test_compile_checked_dict_type_specified(self):
#     codestr = """
#         from __static__ import CheckedDict
#         class B: pass
#         class D(B): pass
#         def testfunc():
#             x: CheckedDict[B, int] = CheckedDict[B, int]({D():42})
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         B = mod.B
#         self.assertEqual(type(test()), chkdict[B, int])
