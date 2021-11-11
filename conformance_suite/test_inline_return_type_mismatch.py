# test_inline_return_type_mismatch.py
# This should fail.

from __static__ import inline
@inline
def f() -> int:
    return 1
def g() -> str:
    return f()
# def test_inline_return_type_mismatch(self):
#     codestr = """
#         from __static__ import inline
#         @inline
#         def f() -> int:
#             return 1
#         def g() -> str:
#             return f()
#     """
#     with self.assertRaisesRegex(TypedSyntaxError, bad_ret_type("int", "str")):
#         self.compile(codestr)
