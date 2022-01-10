# test_unknown_isinstance_narrows.py
# This should pass.

from typing import Any
class C:
    def __init__(self, x: str):
        self.x: str = x
def testfunc(x):
    if isinstance(x, C):
        return x.x
# def test_unknown_isinstance_narrows(self):
#     codestr = """
#         from typing import Any
#         class C:
#             def __init__(self, x: str):
#                 self.x: str = x
#         def testfunc(x):
#             if isinstance(x, C):
#                 return x.x
#     """
#     with self.in_module(codestr) as mod:
#         testfunc = mod.testfunc
#         self.assertInBytecode(testfunc, "LOAD_FIELD", (mod.__name__, "C", "x"))
