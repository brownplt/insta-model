# Reason: Test hitted a banned word global
def test_fast_for_iter_global(self):
    codestr = """
        for i in [1,2,3]:
            X = i
    """
    code = self.compile(codestr)
    self.assertInBytecode(code, "FAST_LEN")
    self.assertEqual(code.co_nlocals, 1)
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.X, 3)
