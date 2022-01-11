# test_if_optional_reassign.py
# This should pass.

# We added the next line because `Optional` should have been imported.
from typing import Optional

class C: pass
def testfunc(abc: Optional[C]):
    if abc is not None:
        abc = None
# def test_if_optional_reassign(self):
#     codestr = """
#     class C: pass
#     def testfunc(abc: Optional[C]):
#         if abc is not None:
#             abc = None
#     """
#     code = self.compile(codestr, modname="foo")
