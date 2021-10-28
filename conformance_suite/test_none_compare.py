# test_none_compare.py
# This should fail.

def f(x: int | None):
    if x > 1:
        x = 1
    return x
# def test_none_compare(self):
#     codestr = """
#         def f(x: int | None):
#             if x > 1:
#                 x = 1
#             return x
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         r"'>' not supported between 'Optional\[int\]' and 'Literal\[1\]'",
#     ):
#         self.compile(codestr)
