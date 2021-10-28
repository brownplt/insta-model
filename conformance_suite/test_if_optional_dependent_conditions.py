# test_if_optional_dependent_conditions.py
# This should pass.

from typing import Optional
class C:
    def __init__(self):
        self.field: Optional[C] = None
def f(x: Optional[C]) -> C:
    if x is not None and x.field is not None:
        return x
    if x is None:
        return C()
    return x
# def test_if_optional_dependent_conditions(self):
#     codestr = """
#         from typing import Optional
#         class C:
#             def __init__(self):
#                 self.field: Optional[C] = None
#         def f(x: Optional[C]) -> C:
#             if x is not None and x.field is not None:
#                 return x
#             if x is None:
#                 return C()
#             return x
#     """
#     self.compile(codestr, modname="foo")
