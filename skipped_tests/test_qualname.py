# Reason: Test hitted some skipped words
def test_qualname(self):
    codestr = """
    def f():
        pass
    class C:
        def x(self):
            pass
        @staticmethod
        def sm():
            pass
        @classmethod
        def cm():
            pass
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        C = mod.C
        self.assertEqual(cinder._get_qualname(f.__code__), "f")
        self.assertEqual(cinder._get_qualname(C.x.__code__), "C.x")
        self.assertEqual(cinder._get_qualname(C.sm.__code__), "C.sm")
        self.assertEqual(cinder._get_qualname(C.cm.__code__), "C.cm")
