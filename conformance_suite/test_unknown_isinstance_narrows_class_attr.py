# test_unknown_isinstance_narrows_class_attr.py
# This should pass.

from typing import Any
class C:
    def __init__(self, x: str):
        self.x: str = x
    def f(self, other) -> str:
        if isinstance(other, self.__class__):
            return other.x
        return ''
# def test_unknown_isinstance_narrows_class_attr(self):
#     codestr = """
#         from typing import Any
#         class C:
#             def __init__(self, x: str):
#                 self.x: str = x
#             def f(self, other) -> str:
#                 if isinstance(other, self.__class__):
#                     return other.x
#                 return ''
#     """
#     with self.in_module(codestr) as mod:
#         C = mod.C
#         self.assertInBytecode(
#             C.f,
#             "LOAD_FIELD",
#             (mod.__name__, "C", "x"),
#         )
