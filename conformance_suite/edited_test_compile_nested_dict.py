# test_compile_nested_dict.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
class B: pass
class D(B): pass
def testfunc():
    x = CheckedDict[B, int]({B():42, D():42})
    y = CheckedDict[int, CheckedDict[B, int]]({42: x})
    return y

# The next line is edited because we don't support first-class classes.
# def main(test, B):
def main(test):
    assert type(test()) == CheckedDict[int, CheckedDict[B, int]]

# The next line is edited because we don't support first-class classes.
# main(testfunc, B)
main(testfunc)

# def test_compile_nested_dict(self):
#     codestr = """
#         from __static__ import CheckedDict
#         class B: pass
#         class D(B): pass
#         def testfunc():
#             x = CheckedDict[B, int]({B():42, D():42})
#             y = CheckedDict[int, CheckedDict[B, int]]({42: x})
#             return y
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         B = mod.B
#         self.assertEqual(type(test()), chkdict[int, chkdict[B, int]])
