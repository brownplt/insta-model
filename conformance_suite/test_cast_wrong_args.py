# test_cast_wrong_args.py
# This should fail.

from __static__ import cast
def f():
    cast(42)
# def test_cast_wrong_args(self):
#     codestr = """
#         from __static__ import cast
#         def f():
#             cast(42)
#     """
#     with self.assertRaises(TypedSyntaxError):
#         self.compile(codestr)
