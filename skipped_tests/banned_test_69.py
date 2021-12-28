# Reason: Test hitted a banned word int64
def test_unbox_typed_bool(self):
    codestr = """
    from __static__ import int64, box
    def f(i: object):
        x = int64(i)
        return box(x)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(42), 42)
        self.assertInBytecode(f, "PRIMITIVE_UNBOX")
        self.assertEqual(f(True), 1)
        self.assertEqual(f(False), 0)
