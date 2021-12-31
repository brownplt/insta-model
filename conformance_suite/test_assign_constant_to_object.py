# test_assign_constant_to_object.py
# This should pass.
# This should terminate.
# This should be optimized.

def f():
    x: object = 42 + 1
# def test_assign_constant_to_object(self):
#     codestr = """
#         def f():
#             x: object = 42 + 1
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
