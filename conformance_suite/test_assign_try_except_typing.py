# test_assign_try_except_typing.py
# This should pass.

def testfunc():
    try:
        pass
    except Exception as e:
        pass
    return 42
# def test_assign_try_except_typing(self):
#     codestr = """
#         def testfunc():
#             try:
#                 pass
#             except Exception as e:
#                 pass
#             return 42
#     """
#     # We don't do anything special w/ Exception type yet, but it should compile
#     code = self.compile(codestr, modname="foo")
