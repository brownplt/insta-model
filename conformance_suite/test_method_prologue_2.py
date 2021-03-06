# test_method_prologue_2.py
# This should pass.
# This should terminate.

def f(x, y: str):
    return 42
def main(f):
    try:
        f('abc', 42)
    except TypeError:
        pass
    else:
        raise Exception()

main(f)
# def test_method_prologue_2(self):
#     codestr = """
#     def f(x, y: str):
#         return 42
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "CHECK_ARGS", (1, ("builtins", "str")))
#         with self.assertRaisesRegex(
#             TypeError, ".*expected 'str' for argument y, got 'int'"
#         ):
#             f("abc", 42)
