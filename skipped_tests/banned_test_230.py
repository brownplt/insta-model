# Reason: Test hitted a banned word int8
def test_array_inplace_assign(self):
    codestr = """
        from __static__ import Array, int8
        def m() -> Array[int8]:
            a = Array[int8]([1, 3, -5, -1, 7, 22])
            a[0] += 1
            return a
    """
    with self.in_module(codestr) as mod:
        m = mod.m
        self.assertEqual(m()[0], 2)
