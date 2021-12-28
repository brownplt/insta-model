# Reason: Test hitted a banned word test_override_override_inherited
def test_override_override_inherited(self):
    codestr = """
    from typing import Optional
    class B:
        def f(self) -> "Optional[B]":
            return self
    class D(B):
        pass
    def f(x: B):
        return x.f()
    """
    with self.in_module(codestr) as mod:
        B = mod.B
        D = mod.D
        f = mod.f
        b = B()
        d = D()
        self.assertEqual(f(b), b)
        self.assertEqual(f(d), d)
        D.f = lambda self: None
        self.assertEqual(f(b), b)
        self.assertEqual(f(d), None)
