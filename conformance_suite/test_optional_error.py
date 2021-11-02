# test_optional_error.py
# This should fail.

from typing import Optional
class C:
    x: Optional["C"]
    def __init__(self, set):
        if set:
            self.x = self
        else:
            self.x = None
    def f(self) -> Optional["C"]:
        return self.x.x
# def test_optional_error(self):
#     codestr = """
#         from typing import Optional
#         class C:
#             x: Optional["C"]
#             def __init__(self, set):
#                 if set:
#                     self.x = self
#                 else:
#                     self.x = None
#             def f(self) -> Optional["C"]:
#                 return self.x.x
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         re.escape("Optional[foo.C]: 'NoneType' object has no attribute 'x'"),
#     ):
#         self.compile(codestr, modname="foo")
