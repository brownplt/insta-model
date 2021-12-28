# Reason: Test hitted a banned word _kw
def test_verify_lambda_kwarg(self):
    codestr = """
        x = lambda **kwargs: kwargs["key"]
        a = x(key="hi")
    """
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.a, "hi")
