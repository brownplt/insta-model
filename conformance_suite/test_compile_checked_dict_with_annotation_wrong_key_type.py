# test_compile_checked_dict_with_annotation_wrong_key_type.py
# This should fail.

from __static__ import CheckedDict
class B: pass
def testfunc():
    x: CheckedDict[B, int] = {object():42}
    return x
# def test_compile_checked_dict_with_annotation_wrong_key_type(self):
#     codestr = """
#         from __static__ import CheckedDict
#         class B: pass
#         def testfunc():
#             x: CheckedDict[B, int] = {object():42}
#             return x
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         type_mismatch(
#             "Exact[chkdict[object, Literal[42]]]",
#             "Exact[chkdict[foo.B, int]]",
#         ),
#     ):
#         self.compile(codestr, modname="foo")
