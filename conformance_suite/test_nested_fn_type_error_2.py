# test_nested_fn_type_error_2.py
# This should pass.
# This should terminate.

def f(i: int, j: str, k: int) -> bool:
    def g(k: int) -> bool:
        return k > 0 if j == "gt" else k <= 0
    return g(i)
def main(f):
    try:
        f(1, 2, 3)
    except TypeError:
        pass
    else:
        raise Exception()

main(f)
# def test_nested_fn_type_error_2(self):
#     codestr = """
#     def f(i: int, j: str, k: int) -> bool:
#         def g(k: int) -> bool:
#             return k > 0 if j == "gt" else k <= 0
#         return g(i)
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         with self.assertRaisesRegex(
#             TypeError, r"f expected 'str' for argument j, got 'int'"
#         ):
#             f(1, 2, 3)
