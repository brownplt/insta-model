# Reason: Test hitted a banned word int64
def test_list_assign_subclass(self):
    class mylist(list):
        def __setitem__(self, idx, value):
            return list.__setitem__(self, idx, value + 1)
    codestr = """
        from __static__ import int64, box, clen
        def f(x: list):
            i: int64 = 0
            x[i] = 42
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "SEQUENCE_SET", SEQ_LIST_INEXACT)
        l = mylist([0])
        f(l)
        self.assertEqual(l[0], 43)
