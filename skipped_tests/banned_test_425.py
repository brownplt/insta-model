# Reason: Test hitted a banned word b"
def test_chkdict_del(self):
    codestr = """
    def f():
        x = {}
        x[1] = "a"
        x[2] = "b"
        del x[1]
        return x
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        ret = f()
        self.assertNotIn(1, ret)
        self.assertIn(2, ret)
