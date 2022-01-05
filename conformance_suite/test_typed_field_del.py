# test_typed_field_del.py
# This should pass.
# This should terminate.

class D:
    def __init__(self, counter):
        self.counter = counter
        self.counter[0] += 1
    def __del__(self):
        self.counter[0] -= 1
class C:
    def __init__(self, value: D):
        self.x: D = value
    def __del__(self):
        del self.x
counter = [0]
d = D(counter)
a = C(d)
del d
assert counter[0] == 1
del a
assert counter[0] == 0

# def test_typed_field_del(self):
#     codestr = """
#         class D:
#             def __init__(self, counter):
#                 self.counter = counter
#                 self.counter[0] += 1
#             def __del__(self):
#                 self.counter[0] -= 1
#         class C:
#             def __init__(self, value: D):
#                 self.x: D = value
#             def __del__(self):
#                 del self.x
#     """
#     count = 0
#     with self.in_module(codestr) as mod:
#         D = mod.D
#         counter = [0]
#         d = D(counter)
#         C = mod.C
#         a = C(d)
#         del d
#         self.assertEqual(counter[0], 1)
#         del a
#         self.assertEqual(counter[0], 0)
