# test_if_else_optional_return_two_branches.py
# This should pass.

from typing import Optional
class C:
    def __init__(self):
        self.field = self
def f(x: Optional[C]):
    if x is None:
        # We added three lines below to declare `a`. Otherwise this program
        #   will be rejected because of the unbound identifier.
        def f():
            return 42
        a = f()
        if a:
            return 0
        else:
            return 2
    return x.field
# def test_if_else_optional_return_two_branches(self):
#     codestr = """
#         from typing import Optional
#         class C:
#             def __init__(self):
#                 self.field = self
#         def f(x: Optional[C]):
#             if x is None:
#                 if a:
#                     return 0
#                 else:
#                     return 2
#             return x.field
#     """
#     self.compile(codestr, modname="foo")
