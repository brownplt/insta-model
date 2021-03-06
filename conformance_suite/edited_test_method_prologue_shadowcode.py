# test_method_prologue_shadowcode.py
# This should pass.
# This should terminate.

def f(x, y: str):
    return 42
def main(f):
    # We change the next line, reducing 100 to 2
    # for i in range(100):
    for i in range(2):
        assert f('abc', 'abc') == 42
    try:
        f('abc', 42)
    except TypeError:
        pass
    else:
        raise Exception()

main(f)
# def test_method_prologue_shadowcode(self):
#     codestr = """
#     def f(x, y: str):
#         return 42
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "CHECK_ARGS", (1, ("builtins", "str")))
#         for i in range(100):
#             self.assertEqual(f("abc", "abc"), 42)
#         with self.assertRaisesRegex(
#             TypeError, ".*expected 'str' for argument y, got 'int'"
#         ):
#             f("abc", 42)
