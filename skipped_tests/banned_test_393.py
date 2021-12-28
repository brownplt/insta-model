# Reason: Test hitted a banned word int8
def test_array_get_dynamic_idx(self):
    codestr = """
        from __static__ import Array, int8, box
        def x():
            return 33
        def m() -> int:
            content = list(range(121))
            a = Array[int8](content)
            return box(a[x()])
    """
    with self.in_module(codestr) as mod:
        m = mod.m
        actual = m()
        self.assertEqual(actual, 33)
