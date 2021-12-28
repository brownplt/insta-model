# Reason: Test hitted a banned word float
def test_double_return_with_default_args(self):
    codestr = """
    from __static__ import double
    def fn(x: float, y: float = 3.2) -> double:
        i = double(x)
        j = double(y)
        return i + j
    """
    with self.in_module(codestr) as mod:
        fn = mod.fn
        r = fn(1.2)
        self.assertEqual(r, 4.4)
