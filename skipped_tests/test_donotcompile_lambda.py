# Reason: Test hitted some skipped words
def test_donotcompile_lambda(self):
    codestr = """
    from __static__ import _donotcompile
    def a() -> int:
        return 1
    class C:
        @_donotcompile
        def fn() -> None:
            z = lambda: a() + 2
            z()
        def fn2() -> None:
            z = lambda: a() + 2
            z()
    c = C()
    """
    with self.in_module(codestr) as mod:
        C = mod.C
        fn = C.fn
        lambda_code = self.find_code(fn.__code__)
        self.assertNotInBytecode(lambda_code, "INVOKE_FUNCTION")
        self.assertFalse(lambda_code.co_flags & CO_STATICALLY_COMPILED)
        self.assertEqual(fn(), None)
        fn2 = C.fn2
        lambda_code2 = self.find_code(fn2.__code__)
        self.assertInBytecode(lambda_code2, "INVOKE_FUNCTION")
        self.assertTrue(lambda_code2.co_flags & CO_STATICALLY_COMPILED)
        self.assertEqual(fn2(), None)
