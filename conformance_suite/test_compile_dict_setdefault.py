# test_compile_dict_setdefault.py
# This should fail.

from __static__ import CheckedDict
def testfunc():
    x = CheckedDict[int, str]({42: 'abc', })
    x.setdefault(100, 43)
# def test_compile_dict_setdefault(self):
#     codestr = """
#         from __static__ import CheckedDict
#         def testfunc():
#             x = CheckedDict[int, str]({42: 'abc', })
#             x.setdefault(100, 43)
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         r"Literal\[43\] received for positional arg 2, expected Optional\[str\]",
#     ):
#         self.compile(codestr, modname="foo")
