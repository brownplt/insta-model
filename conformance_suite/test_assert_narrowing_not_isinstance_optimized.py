# test_assert_narrowing_not_isinstance_optimized.py
# This should pass.
# This should terminate.

def foo(x: int | str) -> str:
    assert not isinstance(x, int)
    return x
# def test_assert_narrowing_not_isinstance_optimized(self):
#     # We ensure that the code without the assert would work in the runtime.
#     codestr = """
#     def foo(x: int | str) -> str:
#         assert not isinstance(x, int)
#         return x
#     """
#     with self.in_module(codestr, optimize=1) as mod:
#         foo = mod.foo
#         self.assertEqual(foo("abc"), "abc")
