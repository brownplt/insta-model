# test_code_flags.py
# This should pass.

def func():
    print("hi")
func()
# def test_code_flags(self):
#     codestr = """
#     def func():
#         print("hi")
#     func()
#     """
#     module = self.compile(codestr)
#     self.assertTrue(module.co_flags & CO_STATICALLY_COMPILED)
#     self.assertTrue(
#         self.find_code(module, name="func").co_flags & CO_STATICALLY_COMPILED
#     )
