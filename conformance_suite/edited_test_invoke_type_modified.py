# test_invoke_type_modified.py
# This should pass.
# This should terminate.

class C:
    def f(self):
        return 1
def x(c: C):
    x = c.f()
    x += c.f()
    return x
# We changed the next line to fix the syntax error introduced by our script
#   and to avoid first-class classes.
def main(x):
    assert x(C()) == 2
    C.f = lambda self: 42
    assert x(C()) == 84

# We changed the next line to fix the syntax error introduced by our script
#   and to avoid first-class classes.
main(x)
# def test_invoke_type_modified(self):
#     codestr = """
#         class C:
#             def f(self):
#                 return 1
#         def x(c: C):
#             x = c.f()
#             x += c.f()
#             return x
#     """
#     code = self.compile(codestr, modname="foo")
#     x = self.find_code(code, "x")
#     self.assertInBytecode(x, "INVOKE_METHOD", (("foo", "C", "f"), 0))
#     with self.in_module(codestr) as mod:
#         x, C = mod.x, mod.C
#         self.assertEqual(x(C()), 2)
#         C.f = lambda self: 42
#         self.assertEqual(x(C()), 84)
