# Reason: Test hitted a banned word int64
def test_clen(self):
    codestr = """
        from __static__ import box, clen, int64
        from typing import List
        def f(l: List[int]):
            x: int64 = clen(l)
            return box(x)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "FAST_LEN", FAST_LEN_LIST | FAST_LEN_INEXACT)
        self.assertEqual(f([1, 2, 3]), 3)
        class MyList(list):
            def __len__(self):
                return 99
        self.assertEqual(f(MyList([1, 2])), 99)
