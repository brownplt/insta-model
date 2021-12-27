# test_while_else_reverses_condition.py
# This should pass.

from typing import Optional
def f(x: Optional[int]) -> int:
    while x is None:
        pass
    else:
        return x
    return 1
# def test_while_else_reverses_condition(self):
#     codestr = """
#         from typing import Optional
#         def f(x: Optional[int]) -> int:
#             while x is None:
#                 pass
#             else:
#                 return x
#             return 1
#     """
#     self.compile(codestr, modname="foo")
