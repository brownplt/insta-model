# test_or_expression_with_multiple_optionals_type_error.py
# This should fail.

from typing import Optional
def f(s1: Optional[str], s2: Optional[str]) -> str:
    return s1 or s2
# def test_or_expression_with_multiple_optionals_type_error(self):
#     codestr = """
#     from typing import Optional
#     def f(s1: Optional[str], s2: Optional[str]) -> str:
#         return s1 or s2
#     """
#     self.type_error(codestr, bad_ret_type("Optional[str]", "str"))
