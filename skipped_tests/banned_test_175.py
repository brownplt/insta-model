# Reason: Test hitted a banned word _kw
def test_kwargs_get(self):
    codestr = """
        def test(**foo):
            print(foo.get('bar'))
    """
    with self.in_module(codestr) as mod:
        test = mod.test
        self.assertInBytecode(
            test, "INVOKE_FUNCTION", (("builtins", "dict", "get"), 2)
        )
