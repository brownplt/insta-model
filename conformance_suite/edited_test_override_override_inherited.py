# test_override_override_inherited.py
# This should pass.
# This should terminate.

from typing import Optional
class B:
    # We edited the next line because our model can't parse the whole string.
    # def f(self) -> "Optional[B]":
    def f(self) -> Optional["B"]:
        return self
class D(B):
    pass
def f(x: B):
    return x.f()

# We edited the next line because we don't support first-class classes.
# def main(B, D, f):
def main(f):
    b = B()
    d = D()
    assert f(b) == b
    assert f(d) == d
    D.f = lambda self: None
    assert f(b) == b
    assert f(d) == None

# We edited the next line because we don't support first-class classes.
# main(B, D, f)
main(f)

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
