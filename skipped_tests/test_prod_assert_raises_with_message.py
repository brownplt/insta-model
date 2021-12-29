# Reason: Hitted a skipped word (prod_assert)
def test_prod_assert_raises_with_message(self):
    codestr = """
    from typing import Optional
    from __static__ import prod_assert
    def foo(x: Optional[int]) -> int:
        prod_assert(x, "x must be int")
        return x
    """
    with self.in_module(codestr) as mod:
        foo = mod.foo
        with self.assertRaisesRegex(AssertionError, "x must be int"):
            foo(None)
