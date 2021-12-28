# test_assign_dynamic_to_dynamic.py
# This should fail.

def f(C):
    x: unknown = C()
# def test_assign_dynamic_to_dynamic(self):
#     codestr = """
#         def f(C):
#             x: unknown = C()
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
