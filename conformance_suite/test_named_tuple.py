# test_named_tuple.py
# This should pass.
# This should terminate.


from typing import NamedTuple
class C(NamedTuple):
    x: int
    y: str
def myfunc(x: C):
    return x.x
# def test_named_tuple(self):
#     codestr = """
#         from typing import NamedTuple
#         class C(NamedTuple):
#             x: int
#             y: str
#         def myfunc(x: C):
#             return x.x
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.myfunc
#         self.assertNotInBytecode(f, "LOAD_FIELD")
