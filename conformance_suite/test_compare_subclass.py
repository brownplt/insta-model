# test_compare_subclass.py
# This should pass.
# This should terminate.
# This should be optimized.

class C: pass
class D(C): pass
x = C() > D()
# def test_compare_subclass(self):
#     codestr = """
#     class C: pass
#     class D(C): pass
#     x = C() > D()
#     """
#     code = self.compile(codestr)
#     self.assertInBytecode(code, "COMPARE_OP")
