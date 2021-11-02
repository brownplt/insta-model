# test_return_outside_func.py
# This should fail.

return 42
# def test_return_outside_func(self):
#     codestr = """
#         return 42
#     """
#     with self.assertRaisesRegex(SyntaxError, "'return' outside function"):
#         self.compile(codestr, modname="foo")
