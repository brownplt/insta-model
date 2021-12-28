# Reason: Test hitted a banned word double
def test_double_return_static(self):
    codestr = """
    from __static__ import double, box
    def fn() -> double:
        return double(3.14159)
    def lol():
        return box(fn()) + 1.0
    """
    with self.in_module(codestr) as mod:
        lol = mod.lol
        r = lol()
        self.assertEqual(r, 4.14159)
