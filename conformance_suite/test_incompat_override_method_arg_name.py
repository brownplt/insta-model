# test_incompat_override_method_arg_name.py
# This should fail.

class A:
    def m(self, x: str) -> int:
        return 42
class B(A):
    def m(self, y: str) -> int:
        return 0
# def test_incompat_override_method_arg_name(self):
#     codestr = """
#         class A:
#             def m(self, x: str) -> int:
#                 return 42
#         class B(A):
#             def m(self, y: str) -> int:
#                 return 0
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         "<module>.B.m overrides <module>.A.m inconsistently. "
#         "Positional argument 2 named `x` is overridden as `y`",
#     ):
#         self.compile(codestr)
