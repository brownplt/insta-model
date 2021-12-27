# test_assign_while_test_var.py
# This should pass.

from typing import Optional
def f(x: Optional[int]) -> int:
    while x is None:
        x = 1
    return x
# def test_assign_while_test_var(self):
#     codestr = """
#         from typing import Optional
#         def f(x: Optional[int]) -> int:
#             while x is None:
#                 x = 1
#             return x
#     """
#     self.compile(codestr, modname="foo")
