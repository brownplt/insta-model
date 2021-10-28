# test_static_import_star.py
# This should fail.

from __static__ import *
# def test_static_import_star(self) -> None:
#     codestr = """
#         from __static__ import *
#     """
#     with self.assertRaises(TypedSyntaxError):
#         self.compile(codestr, modname="foo")
