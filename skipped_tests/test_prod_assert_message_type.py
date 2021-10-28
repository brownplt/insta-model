def test_prod_assert_message_type(self):
    codestr = """
    from typing import Optional
    from __static__ import prod_assert
    def foo(x: Optional[int]) -> int:
        prod_assert(x, 3)
        return x
    """
    self.type_error(
        codestr, r"type mismatch: Literal\[3\] cannot be assigned to str"
    )
