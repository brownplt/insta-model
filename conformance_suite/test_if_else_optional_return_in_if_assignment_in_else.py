# test_if_else_optional_return_in_if_assignment_in_else.py
# This should pass.

from typing import Optional
def f(x: Optional[int]) -> int:
    if x is not None:
        return 2
    else:
        x = 1
    return x
# def test_if_else_optional_return_in_if_assignment_in_else(self):
#     codestr = """
#         from typing import Optional
#         def f(x: Optional[int]) -> int:
#             if x is not None:
#                 return 2
#             else:
#                 x = 1
#             return x
#     """
#     self.compile(codestr, modname="foo")
