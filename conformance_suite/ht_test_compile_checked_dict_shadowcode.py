# ht_test_compile_checked_dict_shadowcode.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
class B: pass
class D(B): pass
def testfunc():
    x = CheckedDict[B, int]({B():42, D():42})
    return x
test = testfunc
# EDIT: We changed the number from 200 down to 2.
for i in range(2):
    assert type(test()) == CheckedDict[B, int]

# def test_compile_checked_dict_shadowcode(self):
#     codestr = """
#         from __static__ import CheckedDict
#         class B: pass
#         class D(B): pass
#         def testfunc():
#             x = CheckedDict[B, int]({B():42, D():42})
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         B = mod.B
#         for i in range(200):
#             self.assertEqual(type(test()), chkdict[B, int])
