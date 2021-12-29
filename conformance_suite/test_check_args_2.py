# test_check_args_2.py
# This should pass.
# This should terminate.


def use(i: object) -> object:
    return i
def outer(x: int, y: str) -> object:
    def inner() -> None:
        use(x)
        use(y)
    use(x)
    return use(y)
# def test_check_args_2(self):
#     """
#     Tests whether CHECK_ARGS can handle multiple variables which are in a Cell,
#     and are positional args.
#     """
#     codestr = """
#         def use(i: object) -> object:
#             return i
#         def outer(x: int, y: str) -> object:
#             def inner() -> None:
#                 use(x)
#                 use(y)
#             use(x)
#             return use(y)
#     """
#     with self.in_module(codestr) as mod:
#         outer = mod.outer
#         self.assertEqual(outer(1, "yo"), "yo")
#         # Force JIT-compiled code to go through argument checks after
#         # keyword arg binding
#         self.assertEqual(outer(1, y="yo"), "yo")
