# Reason: Test hitted a banned word _kw
def test_verify_kwdefaults(self):
    codestr = """
        def x(*, b: str="hunter2"):
            return b
        z = x(b="lol")
    """
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.z, "lol")
