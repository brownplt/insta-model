# test_invoke_strict_module_mutual_recursive.py
# This should pass.

# This should be optimized.

def fib1(number):
    if number <= 1:
        return number
    return(fib(number-1) + fib(number-2))
def fib(number):
    if number <= 1:
        return number
    return(fib1(number-1) + fib1(number-2))
# def test_invoke_strict_module_mutual_recursive(self):
#     codestr = """
#         def fib1(number):
#             if number <= 1:
#                 return number
#             return(fib(number-1) + fib(number-2))
#         def fib(number):
#             if number <= 1:
#                 return number
#             return(fib1(number-1) + fib1(number-2))
#     """
#     with self.in_strict_module(codestr) as mod:
#         fib = mod.fib
#         fib1 = mod.fib1
#         self.assertInBytecode(
#             fib,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "fib1"), 1),
#         )
#         self.assertInBytecode(
#             fib1,
#             "INVOKE_FUNCTION",
#             ((mod.__name__, "fib"), 1),
#         )
#         self.assertEqual(fib(0), 0)
#         self.assert_jitted(fib1)
#         self.assertEqual(fib(4), 3)
