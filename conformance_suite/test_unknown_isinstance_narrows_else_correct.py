# test_unknown_isinstance_narrows_else_correct.py
# This should pass.
# This should terminate.

from typing import Any
class C:
    def __init__(self, x: str):
        self.x: str = x
def testfunc(x):
    if isinstance(x, C):
        pass
    else:
        return x.x
# def test_unknown_isinstance_narrows_else_correct(self):
#     codestr = """
#         from typing import Any
#         class C:
#             def __init__(self, x: str):
#                 self.x: str = x
#         def testfunc(x):
#             if isinstance(x, C):
#                 pass
#             else:
#                 return x.x
#     """
#     with self.in_module(codestr) as mod:
#         testfunc = mod.testfunc
#         self.assertNotInBytecode(testfunc, "LOAD_FIELD", (mod.__name__, "C", "x"))
