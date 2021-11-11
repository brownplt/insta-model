# test_assert_narrowing_debug.py
# This should pass.
# This should terminate.


def foo(x: int | str) -> int:
    assert isinstance(x, int)
    return x + 1
# def test_assert_narrowing_debug(self):
#     codestr = """
#     def foo(x: int | str) -> int:
#         assert isinstance(x, int)
#         return x + 1
#     """
#     with self.in_module(codestr) as mod:
#         foo = mod.foo
#         self.assertEqual(foo(1), 2)
#         with self.assertRaises(AssertionError):
#             foo("a")
