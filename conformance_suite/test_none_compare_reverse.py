# test_none_compare_reverse.py
# This should fail.

def f(x: int | None):
    if 1 > x:
        x = 1
    return x
# def test_none_compare_reverse(self):
#     codestr = """
#         def f(x: int | None):
#             if 1 > x:
#                 x = 1
#             return x
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         r"'>' not supported between 'Literal\[1\]' and 'Optional\[int\]'",
#     ):
#         self.compile(codestr)
