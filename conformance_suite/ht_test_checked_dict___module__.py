# ht_test_checked_dict___module__.py
# This should pass.
# This should terminate.

from __static__ import chkdict

class Lol:
    pass
x = chkdict[int, Lol]()
assert type(x).__module__ is "__static__"

# def test_checked_dict___module__(self):
#     class Lol:
#         pass
#     x = chkdict[int, Lol]()
#     self.assertEqual(type(x).__module__, "__static__")
