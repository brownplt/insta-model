# Reason: Hitted a skipped word (prod_assert)
def test_prod_assert_static_error(self):
    codestr = """
    from typing import Optional
    from __static__ import prod_assert
    def foo(x: Optional[int]) -> str:
        prod_assert(x)
        return x
    """
    self.type_error(codestr, "return type must be str, not int")
