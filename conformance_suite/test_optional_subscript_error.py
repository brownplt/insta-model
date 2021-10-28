# test_optional_subscript_error.py
# This should fail.

from typing import Optional
def f(a: Optional[int]):
    a[1]
# def test_optional_subscript_error(self) -> None:
#     codestr = """
#         from typing import Optional
#         def f(a: Optional[int]):
#             a[1]
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         re.escape("Optional[int]: 'NoneType' object is not subscriptable"),
#     ):
#         self.compile(codestr)
