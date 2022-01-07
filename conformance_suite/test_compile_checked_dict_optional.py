# test_compile_checked_dict_optional.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
from typing import Optional
def testfunc():
    x = CheckedDict[str, str | None]({
        'x': None,
        'y': 'z'
    })
    return x
def main(f):
    x = f()
    x['z'] = None
    assert type(x) == CheckedDict[str, str | None]

main(testfunc)
# def test_compile_checked_dict_optional(self):
#     codestr = """
#         from __static__ import CheckedDict
#         from typing import Optional
#         def testfunc():
#             x = CheckedDict[str, str | None]({
#                 'x': None,
#                 'y': 'z'
#             })
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.testfunc
#         x = f()
#         x["z"] = None
#         self.assertEqual(type(x), chkdict[str, str | None])
