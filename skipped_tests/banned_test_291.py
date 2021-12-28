# Reason: Test hitted a banned word f"
def test_static_function_invoke(self):
    codestr = """
        class C:
            @staticmethod
            def f():
                return 42
        def f():
            return C.f()
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "INVOKE_FUNCTION", ((mod.__name__, "C", "f"), 0))
        self.assertEqual(f(), 42)
