# test_incompat_override_method_arg_type.py
# This should fail.

class A:
    def m(self, x: str) -> int:
        return 42
class B(A):
    def m(self, x: int) -> int:
        return 0
# def test_incompat_override_method_arg_type(self):
#     codestr = """
#         class A:
#             def m(self, x: str) -> int:
#                 return 42
#         class B(A):
#             def m(self, x: int) -> int:
#                 return 0
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         "<module>.B.m overrides <module>.A.m inconsistently. "
#         "Parameter x of type `int` is not a subtype of the overridden parameter `str`",
#     ):
#         self.compile(codestr)
