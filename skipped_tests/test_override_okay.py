# Reason: Can't be translated by any of the three translator
def test_override_okay(self):
    codestr = """
    class B:
        def f(self) -> "B":
            return self
    def f(x: B):
        return x.f()
    """
    with self.in_module(codestr) as mod:
        B = mod.B
        f = mod.f
        class D(B):
            def f(self):
                return self
        x = f(D())
