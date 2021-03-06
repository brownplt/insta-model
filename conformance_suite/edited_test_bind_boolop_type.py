# test_bind_boolop_type.py
# This should pass.
# This should terminate.

from typing import Any
class C:
    def f(self) -> bool:
        return True
    def g(self) -> bool:
        return False
    def x(self) -> bool:
        return self.f() and self.g()
    def y(self) -> bool:
        return self.f() or self.g()

# We edited the next line because we don't support first-class classes
# def main(C):
def main():
    c = C()
    assert c.x() == False
    assert c.y() == True

# We edited the next line because we don't support first-class classes
# main(C)
main()

# def test_bind_boolop_type(self):
#     codestr = """
#         from typing import Any
#         class C:
#             def f(self) -> bool:
#                 return True
#             def g(self) -> bool:
#                 return False
#             def x(self) -> bool:
#                 return self.f() and self.g()
#             def y(self) -> bool:
#                 return self.f() or self.g()
#     """
#     with self.in_module(codestr) as mod:
#         C = mod.C
#         c = C()
#         self.assertEqual(c.x(), False)
#         self.assertEqual(c.y(), True)
