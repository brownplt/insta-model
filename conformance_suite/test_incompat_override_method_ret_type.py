# test_incompat_override_method_ret_type.py
# This should fail.

class A:
    def m(self) -> str:
        return "hello"
class B(A):
    def m(self) -> int:
        return 0
# def test_incompat_override_method_ret_type(self):
#     codestr = """
#         class A:
#             def m(self) -> str:
#                 return "hello"
#         class B(A):
#             def m(self) -> int:
#                 return 0
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         "<module>.B.m overrides <module>.A.m inconsistently. "
#         "Returned type `int` is not a subtype of the overridden return `str`",
#     ):
#         self.compile(codestr)
