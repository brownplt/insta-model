# test_verify_positional_args.py
# This should fail.

def x(a: int, b: str) -> None:
    pass
x("a", 2)
# def test_verify_positional_args(self):
#     codestr = """
#         def x(a: int, b: str) -> None:
#             pass
#         x("a", 2)
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         r"Exact\[str\] received for positional arg 'a', expected int",
#     ):
#         self.compile(codestr)
