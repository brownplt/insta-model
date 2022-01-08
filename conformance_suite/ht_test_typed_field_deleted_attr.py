# ht_test_typed_field_deleted_attr.py
# This should pass.
# This should terminate.

class C:
    def __init__(self, value: str):
        self.x: str = value

a = C("abc")
del a.x
try:
    a.x
except AttributeError:
    pass
else:
    raise Exception()

# def test_typed_field_deleted_attr(self):
#     codestr = """
#         class C:
#             def __init__(self, value: str):
#                 self.x: str = value
#     """
#     count = 0
#     with self.in_module(codestr) as mod:
#         C = mod.C
#         a = C("abc")
#         del a.x
#         with self.assertRaises(AttributeError):
#             a.x
