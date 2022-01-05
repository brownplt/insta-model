# test_chkdict_literal.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
def testfunc():
    x: CheckedDict[int,str]  = {}
    return x
f = testfunc
assert type(f()) == CheckedDict[int, str]

# def test_chkdict_literal(self):
#     codestr = """
#         from __static__ import CheckedDict
#         def testfunc():
#             x: CheckedDict[int,str]  = {}
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.testfunc
#         self.assertEqual(type(f()), chkdict[int, str])
