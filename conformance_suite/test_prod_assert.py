# test_prod_assert.py
# This should pass.
# This should terminate.


from typing import Optional
from __static__ import prod_assert
def foo(x: Optional[int]) -> int:
    prod_assert(x)
    return x
# def test_prod_assert(self):
#     codestr = """
#     from typing import Optional
#     from __static__ import prod_assert
#     def foo(x: Optional[int]) -> int:
#         prod_assert(x)
#         return x
#     """
#     with self.in_module(codestr) as mod:
#         foo = mod.foo
#         self.assertEqual(foo(1), 1)
