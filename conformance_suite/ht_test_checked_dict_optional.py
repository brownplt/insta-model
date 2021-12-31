# ht_test_checked_dict_optional.py
# This should pass.
# This should terminate.

from typing import Optional
from __static__ import CheckedDict

x = CheckedDict[str, Optional[str]]()
x["abc"] = None
x = CheckedDict[Optional[str], str]()
x[None] = "abc"

# def test_checked_dict_optional(self):
#     x = chkdict[str, Optional[str]]()
#     x["abc"] = None
#     x = chkdict[Optional[str], str]()
#     x[None] = "abc"
