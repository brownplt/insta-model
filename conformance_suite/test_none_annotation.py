# test_none_annotation.py
# This should fail.

from typing import Optional
def f(x: Optional[int]) -> None:
    return x
# def test_none_annotation(self):
#     codestr = """
#         from typing import Optional
#         def f(x: Optional[int]) -> None:
#             return x
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         bad_ret_type("Optional[int]", "None"),
#     ):
#         self.compile(codestr, modname="foo")
