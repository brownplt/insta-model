# test_unannotated_assign_no_later_declare.py
# This should fail.

def f(flag):
    x = None
    if flag:
        x: str = "foo"
# def test_unannotated_assign_no_later_declare(self) -> None:
#     codestr = """
#         def f(flag):
#             x = None
#             if flag:
#                 x: str = "foo"
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError, r"Cannot redefine local variable x"
#     ):
#         self.compile(codestr)
