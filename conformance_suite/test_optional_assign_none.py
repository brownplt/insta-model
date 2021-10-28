# test_optional_assign_none.py
# This should pass.

from typing import Optional
class B: pass
def f(x: Optional[B]):
    a: Optional[B] = None
# def test_optional_assign_none(self):
#     codestr = """
#         from typing import Optional
#         class B: pass
#         def f(x: Optional[B]):
#             a: Optional[B] = None
#     """
#     self.compile(codestr, modname="foo")
