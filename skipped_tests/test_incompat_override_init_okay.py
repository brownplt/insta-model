def test_incompat_override_init_okay(self):
    codestr = """
        class A:
            def __init__(self) -> None:
                pass
        class B(A):
            def __init__(self, x: int) -> None:
                pass
        def f(x: A):
            x.__init__()
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        # calling __init__ directly shouldn't use INVOKE_METHOD
        # as we allow users to override this inconsistently
        self.assertNotInBytecode(f, "INVOKE_METHOD")
