# test_compile_dict_setdefault_typed.py
# This should pass.

from __static__ import CheckedDict
def testfunc():
    x = CheckedDict[int, str]({42: 'abc', })
    y: str | None = x.setdefault(100, 'foo')
# def test_compile_dict_setdefault_typed(self):
#     codestr = """
#         from __static__ import CheckedDict
#         def testfunc():
#             x = CheckedDict[int, str]({42: 'abc', })
#             y: str | None = x.setdefault(100, 'foo')
#     """
#     self.compile(codestr)
