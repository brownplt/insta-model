# test_verify_arg_unknown_type.py
# This should pass.

# EDIT: We added 1 line to import Any
from typing import Any
# EDIT: We edited the next line to use `Any` in place of `foo`
# def x(x:foo):
def x(x:Any):
    # EDIT: We added 2 lines to declare `b`
    def f(): return 42
    b = f()
    return b
x('abc')
# def test_verify_arg_unknown_type(self):
#     codestr = """
#         def x(x:foo):
#             return b
#         x('abc')
#     """
#     module = self.compile(codestr)
#     self.assertInBytecode(module, "INVOKE_FUNCTION")
#     x = self.find_code(module)
#     self.assertInBytecode(x, "CHECK_ARGS", ())
