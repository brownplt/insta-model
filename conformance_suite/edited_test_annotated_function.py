# test_annotated_function.py
# This should pass.
# This should terminate.

class C:
    def f(self) -> int:
        return 1
def x(c: C):
    x = c.f()
    x += c.f()
    return x
# We changed the next line to fix the syntax error introduced by our script
#   and to avoid first-class classes.
# def main((x, C)):
def main(x):
    c = C()
    assert x(c) == 2

# We changed the next line to fix the syntax error introduced by our script
#   and to avoid first-class classes.
# main((x, C))
main(x)
# def test_annotated_function(self):
#     codestr = """
#     class C:
#         def f(self) -> int:
#             return 1
#     def x(c: C):
#         x = c.f()
#         x += c.f()
#         return x
#     """
#     code = self.compile(codestr, modname="foo")
#     x = self.find_code(code, "x")
#     self.assertInBytecode(x, "INVOKE_METHOD", (("foo", "C", "f"), 0))
#     with self.in_module(codestr) as mod:
#         x, C = mod.x, mod.C
#         c = C()
#         self.assertEqual(x(c), 2)
