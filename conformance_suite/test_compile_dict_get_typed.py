# test_compile_dict_get_typed.py
# This should pass.

from __static__ import CheckedDict
def testfunc():
    x = CheckedDict[int, str]({42: 'abc', })
    y: str | None = x.get(42)