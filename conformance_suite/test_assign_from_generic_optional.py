# test_assign_from_generic_optional.py
# This should fail.

from typing import Optional
class C: pass
def f(x: Optional):
    y: Optional[C] = x
# def test_assign_from_generic_optional(self):
#     codestr = """
#         from typing import Optional
#         class C: pass
#         def f(x: Optional):
#             y: Optional[C] = x
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError, type_mismatch("Optional[T]", optional("foo.C"))
#     ):
#         self.compile(codestr, modname="foo")
