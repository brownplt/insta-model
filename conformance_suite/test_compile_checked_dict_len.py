# test_compile_checked_dict_len.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
def testfunc():
    x = CheckedDict[int, str]({1:'abc'})
    return len(x)
assert test() == 1

# def test_compile_checked_dict_len(self):
#     codestr = """
#         from __static__ import CheckedDict
#         def testfunc():
#             x = CheckedDict[int, str]({1:'abc'})
#             return len(x)
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         self.assertInBytecode(test, "FAST_LEN", FAST_LEN_DICT)
#         if cinderjit is not None:
#             cinderjit.get_and_clear_runtime_stats()
#         self.assertEqual(test(), 1)
#         if cinderjit is not None:
#             stats = cinderjit.get_and_clear_runtime_stats().get("deopt")
#             self.assertFalse(stats)
