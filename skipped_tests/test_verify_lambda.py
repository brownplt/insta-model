def test_verify_lambda(self):
    codestr = """
        x = lambda x: x
        a = x("hi")
    """
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.a, "hi")
