# test_assign_try_except_no_initial.py
# This should pass.
# This should terminate.

class B:
    def f(self):
        return 42
class D(B):
    def f(self):
        return 'abc'
def testfunc():
    try:
        x: B = D()
    except:
        x = B()
    return x.f()
def main(test):
    assert test() == 'abc'

main(testfunc)
# def test_assign_try_except_no_initial(self):
#     codestr = """
#         class B:
#             def f(self):
#                 return 42
#         class D(B):
#             def f(self):
#                 return 'abc'
#         def testfunc():
#             try:
#                 x: B = D()
#             except:
#                 x = B()
#             return x.f()
#     """
#     code = self.compile(codestr, modname="foo")
#     f = self.find_code(code, "testfunc")
#     self.assertInBytecode(f, "INVOKE_METHOD", (("foo", "B", "f"), 0))
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         self.assertEqual(test(), "abc")
