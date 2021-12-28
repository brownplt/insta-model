# Reason: Test hitted a banned word int64
def test_int_unbox_from_call(self):
    codestr = f"""
    from __static__ import int64
    def foo() -> int:
        return 1234
    def testfunc() -> int64:
        return int64(foo())
    """
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        self.assertEqual(f(), 1234)
