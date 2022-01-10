# test_unknown_isinstance_narrows_class_attr_dynamic.py
# This should pass.

from typing import Any
class C:
    def __init__(self, x: str):
        self.x: str = x
    def f(self, other, unknown):
        if isinstance(other, unknown.__class__):
            return other.x
        return ''
# def test_unknown_isinstance_narrows_class_attr_dynamic(self):
#     codestr = """
#         from typing import Any
#         class C:
#             def __init__(self, x: str):
#                 self.x: str = x
#             def f(self, other, unknown):
#                 if isinstance(other, unknown.__class__):
#                     return other.x
#                 return ''
#     """
#     with self.in_module(codestr) as mod:
#         C = mod.C
#         self.assertInBytecode(C.f, "LOAD_ATTR", "x")
