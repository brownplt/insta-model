# test_compile_checked_dict_ann_differs.py
# This should fail.

from __static__ import CheckedDict
def testfunc():
    x: CheckedDict[int, int] = CheckedDict[str, str]({'abc':'abc'})
    return x
# def test_compile_checked_dict_ann_differs(self):
#     codestr = """
#         from __static__ import CheckedDict
#         def testfunc():
#             x: CheckedDict[int, int] = CheckedDict[str, str]({'abc':'abc'})
#             return x
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         type_mismatch(
#             "Exact[chkdict[str, str]]",
#             "Exact[chkdict[int, int]]",
#         ),
#     ):
#         self.compile(codestr, modname="foo")
