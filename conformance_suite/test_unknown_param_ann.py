# test_unknown_param_ann.py
# This should pass.
# This should terminate.

from typing import Any
class C:
    def __init__(self, x: str):
        self.x: str = x
    def __eq__(self, other: Any) -> bool:
        return False
# def test_unknown_param_ann(self):
#     codestr = """
#         from typing import Any
#         class C:
#             def __init__(self, x: str):
#                 self.x: str = x
#             def __eq__(self, other: Any) -> bool:
#                 return False
#     """
#     with self.in_module(codestr) as mod:
#         C = mod.C
#         x = C("abc")
#         self.assertInBytecode(C.__eq__, "CHECK_ARGS", (0, (mod.__name__, "C")))
#         self.assertNotEqual(x, x)
