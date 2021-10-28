# test_optional_assign_subclass.py
# This should pass.

from typing import Optional
class B: pass
class D(B): pass
def f(x: D):
    a: Optional[B] = x
# def test_optional_assign_subclass(self):
#     codestr = """
#         from typing import Optional
#         class B: pass
#         class D(B): pass
#         def f(x: D):
#             a: Optional[B] = x
#     """
#     self.compile(codestr, modname="foo")
