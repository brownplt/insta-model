# Reason: Hitted a skipped word (prod_assert)
def test_prod_assert_keywords_type_error(self):
    codestr = """
    from typing import Optional
    from __static__ import prod_assert
    def foo(x: Optional[int]) -> int:
        prod_assert(x, message="x must be int")
        return x
    """
    self.type_error(codestr, r"prod_assert\(\) does not accept keyword arguments")
