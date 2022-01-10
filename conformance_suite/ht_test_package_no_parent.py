# test_package_no_parent.py
# This should pass.
# This should terminate.

class C:
    def f(self):
        return 42

# We edited the next line because we don't support first-class classes
# def main(C):
def main():
    assert C().f() == 42

# We edited the next line because we don't support first-class classes
# main(C)
main()

# def test_package_no_parent(self):
#     codestr = """
#         class C:
#             def f(self):
#                 return 42
#     """
#     with self.in_module(codestr, name="package_no_parent.child") as mod:
#         C = mod.C
#         self.assertInBytecode(
#             C.f, "CHECK_ARGS", (0, ("package_no_parent.child", "C"))
#         )
#         self.assertEqual(C().f(), 42)
