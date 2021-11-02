# test_none_attribute_error.py
# This should fail.

def f():
    x = None
    return x.foo
# def test_none_attribute_error(self):
#     codestr = """
#         def f():
#             x = None
#             return x.foo
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError, "'NoneType' object has no attribute 'foo'"
#     ):
#         self.compile(codestr, modname="foo")
