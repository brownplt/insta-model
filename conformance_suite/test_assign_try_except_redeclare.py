# test_assign_try_except_redeclare.py
# This should pass.

def testfunc():
    e: int
    try:
        pass
    except Exception as e:
        pass
    return 42
# def test_assign_try_except_redeclare(self):
#     codestr = """
#         def testfunc():
#             e: int
#             try:
#                 pass
#             except Exception as e:
#                 pass
#             return 42
#     """
#     code = self.compile(codestr, modname="foo")
