# Reason: Test hitted some skipped words
def test_prod_assert_raises(self):
    codestr = """
    from typing import Optional
    from __static__ import prod_assert
    def foo(x: Optional[int]) -> int:
        prod_assert(x)
        return x
    """
    with self.in_module(codestr) as mod:
        foo = mod.foo
        with self.assertRaises(AssertionError):
            foo(None)
