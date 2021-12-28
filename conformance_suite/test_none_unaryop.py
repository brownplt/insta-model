# test_none_unaryop.py
# This should fail.

def f():
    x = None
    return -x
# def test_none_unaryop(self):
#     codestr = """
#         def f():
#             x = None
#             return -x
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError, "bad operand type for unary -: 'NoneType'"
#     ):
#         self.compile(codestr, modname="foo")
