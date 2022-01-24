# test_class_method_invoke.py
# This should pass.
# This should terminate.

class B:
    def __init__(self, value):
        self.value = value
class D(B):
    def __init__(self, value):
        B.__init__(self, value)
    def f(self):
        return self.value
# We changed the next line to avoid using first-class classes.
# def main(D):
def main():
    d = D(42)
    assert d.f() == 42

# We changed the next line to avoid using first-class classes.
# main(D)
main()
# def test_class_method_invoke(self):
#     codestr = """
#         class B:
#             def __init__(self, value):
#                 self.value = value
#         class D(B):
#             def __init__(self, value):
#                 B.__init__(self, value)
#             def f(self):
#                 return self.value
#     """
#     code = self.compile(codestr, modname="foo")
#     b_init = self.find_code(self.find_code(code, "B"), "__init__")
#     self.assertInBytecode(b_init, "STORE_FIELD", ("foo", "B", "value"))
#     f = self.find_code(self.find_code(code, "D"), "f")
#     self.assertInBytecode(f, "LOAD_FIELD", ("foo", "B", "value"))
#     with self.in_module(codestr) as mod:
#         D = mod.D
#         d = D(42)
#         self.assertEqual(d.f(), 42)
