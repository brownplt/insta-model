# test_checked_dict_bad_ctor.py
# This should pass.
# This should terminate.

from __static__ import chkdict

try:
    chkdict[str, str](None)
except Exception:
    pass
else:
    raise Exception("Should fail.")

# def test_checked_dict_bad_ctor(self):
#     with self.assertRaises(TypeError):
#         chkdict[str, str](None)
