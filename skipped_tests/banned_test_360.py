# Reason: Test hitted a banned word int32
def test_primitive_return_recursive(self):
    codestr = """
        from __static__ import int32
        def fib(n: int32) -> int32:
            if n <= 1:
                return n
            return fib(n-1) + fib(n-2)
    """
    with self.in_strict_module(codestr) as mod:
        self.assertInBytecode(
            mod.fib,
            "INVOKE_FUNCTION",
            ((mod.__name__, "fib"), 1),
        )
        self.assertEqual(mod.fib(2), 1)
        self.assert_jitted(mod.fib)
