# test_assign_generic_optional_2.py
# This should fail.

from typing import Optional
def f():
    x: Optional = 42 + 1
# def test_assign_generic_optional_2(self):
#     codestr = """
#         from typing import Optional
#         def f():
#             x: Optional = 42 + 1
#     """
#     with self.assertRaises(TypedSyntaxError):
#         self.compile(codestr, modname="foo")
