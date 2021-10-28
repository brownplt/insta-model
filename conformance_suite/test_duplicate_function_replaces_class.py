# test_duplicate_function_replaces_class.py
# This should fail.

class X: pass
def X(): pass
# def test_duplicate_function_replaces_class(self) -> None:
#     codestr = """
#         class X: pass
#         def X(): pass
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError, "function conflicts with other member X in <module>"
#     ):
#         self.compile(codestr)
