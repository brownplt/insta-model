# Reason: Test hitted a banned word int8
def test_inexact_list_negative_small_int(self):
    codestr = """
        from __static__ import int64, box, clen
        def f(x: list):
            i: int8 = 1
            return x[-i]
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        res = f([1, 2, 3])
        self.assertEqual(res, 3)
