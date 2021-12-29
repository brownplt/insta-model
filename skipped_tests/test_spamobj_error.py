# Reason: Hitted a skipped word (xxclassloader)
def test_spamobj_error(self):
    codestr = """
        from xxclassloader import spamobj
        def f():
            x = spamobj[int]()
            return x.error(1)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        with self.assertRaisesRegex(TypeError, "no way!"):
            f()
