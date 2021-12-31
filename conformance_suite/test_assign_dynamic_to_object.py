# test_assign_dynamic_to_object.py
# This should pass.
# This should terminate.
# This should be optimized.

def f(C):
    x: object = C()
# def test_assign_dynamic_to_object(self):
#     codestr = """
#         def f(C):
#             x: object = C()
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
