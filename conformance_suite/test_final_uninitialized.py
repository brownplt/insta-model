# test_final_uninitialized.py
# This should fail.

from typing import Final
x: Final[int]
# def test_final_uninitialized(self):
#     codestr = """
#     from typing import Final
#     x: Final[int]
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError, "Must assign a value when declaring a Final"
#     ):
#         self.compile(codestr, modname="foo")
