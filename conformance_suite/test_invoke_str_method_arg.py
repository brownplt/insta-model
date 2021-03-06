# test_invoke_str_method_arg.py
# This should pass.
# This should terminate.

def func():
    a = 'a b c'
    return a.split('a')
def main(f):
    assert f() == ['', ' b c']

main(func)
# def test_invoke_str_method_arg(self):
#     codestr = """
#     def func():
#         a = 'a b c'
#         return a.split('a')
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.func
#         self.assertInBytecode(
#             f, "INVOKE_FUNCTION", (("builtins", "str", "split"), 2)
#         )
#         self.assertEqual(f(), ["", " b c"])
