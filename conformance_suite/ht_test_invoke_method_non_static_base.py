# test_invoke_method_non_static_base.py
# This should pass.
# This should terminate.

class C(Exception):
    def f(self):
        return 42
    def g(self):
        return self.f()

# We edited the next line because we don't support first-class classes
# def main(C):
def main():
    assert C().g() == 42

# We edited the next line because we don't support first-class classes
# main(C)
main()

# def test_invoke_method_non_static_base(self):
#     codestr = """
#     class C(Exception):
#         def f(self):
#             return 42
#         def g(self):
#             return self.f()
#     """
#     with self.in_module(codestr) as mod:
#         C = mod.C
#         self.assertEqual(C().g(), 42)
