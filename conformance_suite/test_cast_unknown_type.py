# test_cast_unknown_type.py
# This should fail.

from __static__ import cast
def f():
    cast(abc, 42)
# def test_cast_unknown_type(self):
#     codestr = """
#         from __static__ import cast
#         def f():
#             cast(abc, 42)
#     """
#     with self.assertRaises(TypedSyntaxError):
#         self.compile(codestr)
