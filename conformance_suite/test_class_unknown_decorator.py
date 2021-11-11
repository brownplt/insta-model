# test_class_unknown_decorator.py
# This should pass.
# This should terminate.


def dec(f):
    return f
@dec
class C:
    @dec
    def foo(self) -> int:
        return 3
    def f(self):
        return self.foo()
# def test_class_unknown_decorator(self):
#     codestr = """
#         def dec(f):
#             return f
#         @dec
#         class C:
#             @dec
#             def foo(self) -> int:
#                 return 3
#             def f(self):
#                 return self.foo()
#     """
#     with self.in_module(codestr, name="mymod") as mod:
#         C = mod.C
#         self.assertEqual(C().f(), 3)
