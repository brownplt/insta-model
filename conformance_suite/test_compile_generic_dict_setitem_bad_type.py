# test_compile_generic_dict_setitem_bad_type.py
# This should fail.

from __static__ import CheckedDict
def testfunc():
    x = CheckedDict[str, int]({"abc": 42})
    x[42] = 42
# def test_compile_generic_dict_setitem_bad_type(self):
#     codestr = """
#         from __static__ import CheckedDict
#         def testfunc():
#             x = CheckedDict[str, int]({"abc": 42})
#             x[42] = 42
#     """
#     with self.assertRaisesRegex(
#         TypedSyntaxError,
#         type_mismatch("Literal[42]", "str"),
#     ):
#         self.compile(codestr, modname="foo")
