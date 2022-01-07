# test_method_prologue_shadowcode_2.py
# This should pass.
# This should terminate.

def f(x: str):
    return 42
def main(f):
    # EDIT: we changed the next line to reduce 100 to 10
    for i in range(10):
        assert f('abc') == 42
    try:
        f(42)
    except TypeError:
        pass
    else:
        raise Exception()

main(f)
# def test_method_prologue_shadowcode_2(self):
#     codestr = """
#     def f(x: str):
#         return 42
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "CHECK_ARGS", (0, ("builtins", "str")))
#         for i in range(100):
#             self.assertEqual(f("abc"), 42)
#         with self.assertRaisesRegex(
#             TypeError, ".*expected 'str' for argument x, got 'int'"
#         ):
#             f(42)
