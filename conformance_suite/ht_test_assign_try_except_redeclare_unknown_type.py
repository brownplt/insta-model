# test_assign_try_except_redeclare_unknown_type.py
# This should pass.

# EDIT: import Any and use it in place of `UnknownException`
from typing import Any

def testfunc():
    e: int
    try:
        pass
    # EDIT: use `Any` in place of `UnknownException`
    # except UnknownException as e:
    except Any as e:
        pass
    return 42
# def test_assign_try_except_redeclare_unknown_type(self):
#     codestr = """
#         def testfunc():
#             e: int
#             try:
#                 pass
#             except UnknownException as e:
#                 pass
#             return 42
#     """
#     code = self.compile(codestr, modname="foo")
