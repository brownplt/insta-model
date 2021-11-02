# test_final_reassigned_in_tuple.py
# This should fail.

from typing import Final
x: Final[int] = 0xdeadbeef
y = 3
x, y = 4, 5
# def test_final_reassigned_in_tuple(self):
#     codestr = """
#     from typing import Final
#     x: Final[int] = 0xdeadbeef
#     y = 3
#     x, y = 4, 5
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError, "Cannot assign to a Final variable"
#     ):
#         self.compile(codestr, modname="foo")
