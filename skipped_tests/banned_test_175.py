# Reason: Test hitted a banned word test_verify_lambda_keyword_only
def test_verify_lambda_keyword_only(self):
    codestr = """
        x = lambda *, x: x
        a = x(x="hi")
    """
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.a, "hi")
