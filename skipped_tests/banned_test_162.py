# Reason: Test hitted a banned word _kw
def test_invoke_kws(self):
    codestr = """
    class C:
        def f(self, a):
            return a
    def func():
        a = C()
        return a.f(a=2)
    """
    with self.in_module(codestr) as mod:
        f = mod.func
        self.assertEqual(f(), 2)
