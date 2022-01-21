# test_duplicate_function_replaces_function.py
# This should fail.

def f(): pass
def f(): pass
# def test_duplicate_function_replaces_function(self) -> None:
#     codestr = """
#         def f(): pass
#         def f(): pass
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         "function 'function <module>.f' conflicts with other member",
#     ):
#         self.compile(codestr)
