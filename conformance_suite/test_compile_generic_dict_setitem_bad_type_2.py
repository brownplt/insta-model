# test_compile_generic_dict_setitem_bad_type_2.py
# This should fail.

from __static__ import CheckedDict
def testfunc():
    x = CheckedDict[str, int]({"abc": 42})
    x["foo"] = "abc"
# def test_compile_generic_dict_setitem_bad_type_2(self):
#     codestr = """
#         from __static__ import CheckedDict
#         def testfunc():
#             x = CheckedDict[str, int]({"abc": 42})
#             x["foo"] = "abc"
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         type_mismatch("Exact[str]", "int"),
#     ):
#         self.compile(codestr, modname="foo")
