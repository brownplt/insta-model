# Reason: Test hitted a banned word int64
def test_optional_array_enum(self):
    codestr = """
        from __static__ import Array, clen, int64, box
        from typing import Optional
        def f(x: Optional[Array[int64]]):
            if x is None:
                return 42
            i: int64 = 0
            j: int64 = 0
            while i < clen(x):
                j += x[i]
                i+=1
            return box(j)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        a = Array[int64]([1, 2, 3, 4])
        self.assertEqual(f(a), 10)
        self.assertEqual(f(None), 42)
