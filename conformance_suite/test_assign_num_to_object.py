# test_assign_num_to_object.py
# This should fail.

def f():
    x: object = 42
# def test_assign_num_to_object(self):
#     codestr = """
#         def f():
#             x: object = 42
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
