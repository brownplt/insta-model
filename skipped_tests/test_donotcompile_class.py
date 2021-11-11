# Reason: Test hitted some skipped words
def test_donotcompile_class(self):
    codestr = """
    from __static__ import _donotcompile
    def a() -> int:
        return 1
    @_donotcompile
    class C:
        def fn() -> None:
            a() + 2
    @_donotcompile
    class D:
        a()
    c = C()
    """
    with self.in_module(codestr) as mod:
        C = mod.C
        fn = C.fn
        self.assertInBytecode(fn, "CALL_FUNCTION")
        self.assertNotInBytecode(fn, "INVOKE_FUNCTION")
        self.assertFalse(fn.__code__.co_flags & CO_STATICALLY_COMPILED)
        self.assertEqual(fn(), None)
        D = mod.D
