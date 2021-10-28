def test_prod_assert_argcount_type_error(self):
    codestr = """
    from typing import Optional
    from __static__ import prod_assert
    def foo(x: Optional[int]) -> int:
        prod_assert(x, 3, 2)
        return x
    """
    self.type_error(
        codestr, r"prod_assert\(\) must be called with one or two arguments"
    )
