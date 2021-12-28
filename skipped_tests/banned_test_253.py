# Reason: Test hitted a banned word int64
def test_inexact_list_negative(self):
    codestr = """
        from __static__ import int64, box, clen
        def f(x: list):
            i: int64 = 1
            return x[-i]
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "SEQUENCE_GET", SEQ_LIST_INEXACT)
        res = f([1, 2, 3])
        self.assertEqual(res, 3)
