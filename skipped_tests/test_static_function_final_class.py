# Reason: Test hitted some skipped words
def test_static_function_final_class(self):
    codestr = """
        from typing import final
        @final
        class A:
            @staticmethod
            def m() -> int:
                return 42
        def make_a() -> A:
            return A()
        def f() -> int:
            return make_a().m()
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "INVOKE_METHOD")
        self.assertEqual(f(), 42)
