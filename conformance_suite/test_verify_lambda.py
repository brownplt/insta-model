# test_verify_lambda.py
# This should pass.
# This should terminate.

x = lambda x: x
a = x("hi")
def main():
    assert a == 'hi'

main()
# def test_verify_lambda(self):
#     codestr = """
#         x = lambda x: x
#         a = x("hi")
#     """
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.a, "hi")
