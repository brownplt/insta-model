# test_assign_try_except_redeclare_unknown_type.py
# This should pass.

# We added the next line.
from typing import Any

def testfunc():
    e: int
    try:
        pass
    # We edited the next line to use `Any` in place of `UnknownException`
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
