# Reason: Test hitted a banned word double
def test_double_return(self):
    codestr = """
    from __static__ import double
    def fn() -> double:
        return double(3.14159)
    """
    with self.in_module(codestr) as mod:
        fn = mod.fn
        r = fn()
        self.assertEqual(r, 3.14159)
