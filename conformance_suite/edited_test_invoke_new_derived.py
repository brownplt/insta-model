# test_invoke_new_derived.py
# This should pass.
# This should terminate.

class C:
    def f(self):
        return 1
def x(c: C):
    x = c.f()
    x += c.f()
    return x
a = x(C())
class D(C):
    def f(self):
        return 2
b = x(D())
# We changed the next line to fix the syntax error introduced by our script.
def main(a, b):
    assert a == 2
    assert b == 4

# We changed the next line to fix the syntax error introduced by our script.
main(a, b)
# def test_invoke_new_derived(self):
#     codestr = """
#         class C:
#             def f(self):
#                 return 1
#         def x(c: C):
#             x = c.f()
#             x += c.f()
#             return x
#         a = x(C())
#         class D(C):
#             def f(self):
#                 return 2
#         b = x(D())
#     """
#     code = self.compile(codestr, modname="foo")
#     x = self.find_code(code, "x")
#     self.assertInBytecode(x, "INVOKE_METHOD", (("foo", "C", "f"), 0))
#     with self.in_module(codestr) as mod:
#         a, b = mod.a, mod.b
#         self.assertEqual(a, 2)
#         self.assertEqual(b, 4)
