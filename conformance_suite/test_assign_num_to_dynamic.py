# test_assign_num_to_dynamic.py
# This should fail.

def f():
    x: foo = 42
# def test_assign_num_to_dynamic(self):
#     codestr = """
#         def f():
#             x: foo = 42
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
