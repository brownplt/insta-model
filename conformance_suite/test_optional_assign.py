# test_optional_assign.py
# This should pass.

from typing import Optional
class C:
    def f(self, x: Optional["C"]):
        if x is None:
            return self
        else:
            p: Optional["C"] = x
# def test_optional_assign(self):
#     codestr = """
#         from typing import Optional
#         class C:
#             def f(self, x: Optional["C"]):
#                 if x is None:
#                     return self
#                 else:
#                     p: Optional["C"] = x
#     """
#     self.compile(codestr, modname="foo")
