# Reason: Test hitted a banned word int8
def test_array_get_failure(self):
    codestr = """
        from __static__ import Array, int8, box
        def m() -> int:
            a = Array[int8]([1, 3, -5])
            return box(a[20])
    """
    with self.in_module(codestr) as mod:
        m = mod.m
        with self.assertRaisesRegex(IndexError, "index out of range"):
            m()
