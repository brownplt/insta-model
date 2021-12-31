# test_unknown_isinstance_bool_ret.py
# This should pass.
# This should terminate.

from typing import Any
class C:
    def __init__(self, x: str):
        self.x: str = x
    def __eq__(self, other: Any) -> bool:
        return isinstance(other, C)
# def test_unknown_isinstance_bool_ret(self):
#     codestr = """
#         from typing import Any
#         class C:
#             def __init__(self, x: str):
#                 self.x: str = x
#             def __eq__(self, other: Any) -> bool:
#                 return isinstance(other, C)
#     """
#     with self.in_module(codestr) as mod:
#         C = mod.C
#         x = C("abc")
#         y = C("foo")
#         self.assertTrue(x == y)
