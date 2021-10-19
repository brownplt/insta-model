# test_compile_dict_get_typed.py
# This should pass.

from typing import Optional
from __static__ import CheckedDict

def testfunc():
    x = CheckedDict[int, str]({42: 'abc', })
    y: Optional[str] = x.get(42)