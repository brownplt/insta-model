# test_while_optional_cond.py
# This should pass.

from typing import Optional
class C:
    def __init__(self):
        self.field: Optional["C"] = self
def f(x: Optional[C]):
    while x is not None:
        val: Optional[C] = x.field
        if val is not None:
            x = val
# def test_while_optional_cond(self):
#     codestr = """
#         from typing import Optional
#         class C:
#             def __init__(self):
#                 self.field: Optional["C"] = self
#         def f(x: Optional[C]):
#             while x is not None:
#                 val: Optional[C] = x.field
#                 if val is not None:
#                     x = val
#     """
#     self.compile(codestr, modname="foo")
