# test_static_import_unknown.py
# This should fail.

from __static__ import does_not_exist
# def test_static_import_unknown(self) -> None:
#     codestr = """
#         from __static__ import does_not_exist
#     """
#     with self.assertRaises(TypedSyntaxError):
#         self.compile(codestr, modname="foo")
