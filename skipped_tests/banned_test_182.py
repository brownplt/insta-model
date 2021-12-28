# Reason: Test hitted a banned word _kw
def test_verify_kwonly_self_loaded_once(self):
    codestr = """
        class C:
            def x(self, *, a: int=1) -> int:
                return 43
        def f():
            return C().x(a=1)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        io = StringIO()
        dis.dis(f, file=io)
        self.assertEqual(1, io.getvalue().count("TP_ALLOC"))
