# Reason: Test hitted a banned word int64
def test_unbox_str(self):
    codestr = """
    from __static__ import unbox, int64
    def f():
        x:int64 = unbox('abc')
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        with self.assertRaisesRegex(TypeError, "expected 'int', got 'str'"):
            f()
