# Reason: Test hitted a banned word global
def test_global_int(self):
    codestr = """
        X: int =  60 * 60 * 24
    """
    code = self.compile(codestr, modname="foo")
    with self.in_module(codestr) as mod:
        X = mod.X
        self.assertEqual(X, 60 * 60 * 24)
