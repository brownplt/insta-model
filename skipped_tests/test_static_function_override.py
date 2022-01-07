# Reason: Hitted a skipped word (@staticmethod)
def test_static_function_override(self):
    codestr = """
        class A:
            @staticmethod
            def m() -> int:
                return 42
        class B(A):
            @staticmethod
            def m() -> int:
                return 0
        def make_a() -> A:
            return B()
        def f() -> int:
            return make_a().m()
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "INVOKE_METHOD")
        self.assertEqual(f(), 0)
