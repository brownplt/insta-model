# ht_test_checked_dict_nonoptional.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
from typing import Any, Optional

def as_dyn(x):
    return x

x: Any = CheckedDict[str, Optional[str]]()
try:
    x[as_dyn(None)] = "abc"
except TypeError:
    pass
else:
    raise Exception()

x = CheckedDict[Optional[str], str]()
try:
    x["abc"] = as_dyn(None)
except TypeError:
    pass
else:
    raise Exception()

# def test_checked_dict_nonoptional(self):
#     x = chkdict[str, Optional[str]]()
#     with self.assertRaises(TypeError):
#         x[None] = "abc"
#     x = chkdict[Optional[str], str]()
#     with self.assertRaises(TypeError):
#         x["abc"] = None
