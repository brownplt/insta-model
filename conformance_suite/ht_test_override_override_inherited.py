# test_override_override_inherited.py
# This should pass.
# This should terminate.

from typing import Optional
class B:
    # EDIT: We changed the next line.
    # def f(self) -> "Optional[B]":
    def f(self) -> Optional[B]:
        return self
class D(B):
    pass
def f(x: B):
    return x.f()
def main(B, D, f):
    b = B()
    d = D()
    assert f(b) == b
    assert f(d) == d
    D.f = lambda self: None
    assert f(b) == b
    assert f(d) == None

main(B, D, f)
# def test_override_override_inherited(self):
#     codestr = """
#     from typing import Optional
#     class B:
#         def f(self) -> "Optional[B]":
#             return self
#     class D(B):
#         pass
#     def f(x: B):
#         return x.f()
#     """
#     with self.in_module(codestr) as mod:
#         B = mod.B
#         D = mod.D
#         f = mod.f
#         b = B()
#         d = D()
#         self.assertEqual(f(b), b)
#         self.assertEqual(f(d), d)
#         D.f = lambda self: None
#         self.assertEqual(f(b), b)
#         self.assertEqual(f(d), None)
