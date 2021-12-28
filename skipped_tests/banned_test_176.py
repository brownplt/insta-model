# Reason: Test hitted a banned word vararg
def test_verify_lambda_vararg(self):
    codestr = """
        x = lambda *x: x[1]
        a = x(1, "hi")
    """
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.a, "hi")
