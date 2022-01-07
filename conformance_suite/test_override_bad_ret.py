# test_override_bad_ret.py
# This should pass.
# This should terminate.

class B:
    def f(self) -> "B":
        return self
def f(x: B):
    return x.f()
def main(B, f):
    class D(B):
    
        def f(self):
            return 42
    try:
        f(D())
    except TypeError:
        pass
    else:
        raise Exception()

main(B, f)
# def test_override_bad_ret(self):
#     codestr = """
#     class B:
#         def f(self) -> "B":
#             return self
#     def f(x: B):
#         return x.f()
#     """
#     with self.in_module(codestr) as mod:
#         B = mod.B
#         f = mod.f
#         class D(B):
#             def f(self):
#                 return 42
#         with self.assertRaisesRegex(
#             TypeError, "unexpected return type from D.f, expected B, got int"
#         ):
#             f(D())
