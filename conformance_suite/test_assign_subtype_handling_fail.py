# test_assign_subtype_handling_fail.py
# This should fail.

class B: pass
class D(B): pass
def f():
    d: D = D()
    d = B()
# def test_assign_subtype_handling_fail(self):
#     codestr = """
#         class B: pass
#         class D(B): pass
#         def f():
#             d: D = D()
#             d = B()
#     """
#     with self.assertRaisesRegex(TypedSyntaxError, type_mismatch("foo.B", "foo.D")):
#         self.compile(codestr, modname="foo")
