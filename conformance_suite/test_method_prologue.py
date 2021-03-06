# test_method_prologue.py
# This should pass.
# This should terminate.

def f(x: str):
    return 42
def main(f):
    try:
        f(42)
    except TypeError:
        pass
    else:
        raise Exception()

main(f)
# def test_method_prologue(self):
#     codestr = """
#     def f(x: str):
#         return 42
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "CHECK_ARGS", (0, ("builtins", "str")))
#         with self.assertRaisesRegex(
#             TypeError, ".*expected 'str' for argument x, got 'int'"
#         ):
#             f(42)
