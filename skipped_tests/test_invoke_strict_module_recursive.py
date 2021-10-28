def test_invoke_strict_module_recursive(self):
    codestr = """
        def fib(number):
            if number <= 1:
                return number
            return(fib(number-1) + fib(number-2))
    """
    with self.in_strict_module(codestr) as mod:
        fib = mod.fib
        self.assertInBytecode(
            fib,
            "INVOKE_FUNCTION",
            ((mod.__name__, "fib"), 1),
        )
        self.assertEqual(fib(4), 3)
