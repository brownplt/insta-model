# Reason: Hitted a skipped word (@property)
def test_decorated_function_ignored_class(self):
    codestr = """
        class C:
            @property
            def x(self):
                return lambda: 42
            def y(self):
                return self.x()
    """
    with self.in_module(codestr) as mod:
        C = mod.C
        self.assertNotInBytecode(C.y, "INVOKE_METHOD")
        self.assertEqual(C().y(), 42)
