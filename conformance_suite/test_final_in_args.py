# test_final_in_args.py
# This should fail.

from typing import Final
def f(a: Final) -> None:
    pass
# def test_final_in_args(self):
#     codestr = """
#     from typing import Final
#     def f(a: Final) -> None:
#         pass
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         "Final annotation is only valid in initial declaration",
#     ):
#         self.compile(codestr, modname="foo")
