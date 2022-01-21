# test_frozenset_constant.py
# This should pass.

from __static__ import inline
@inline
def i(s: str) -> bool:
    return i in {"a", "b"}
def t() -> bool:
    return i("p")
# def test_frozenset_constant(self):
#     codestr = """
#     from __static__ import inline
#     @inline
#     def i(s: str) -> bool:
#         return i in {"a", "b"}
#     def t() -> bool:
#         return i("p")
#     """
#     self.compile(codestr, modname="foo")
