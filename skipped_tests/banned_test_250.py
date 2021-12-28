# Reason: Test hitted a banned word int64
def test_user_enumerate_list_nooverride(self):
    class mylist(list):
        pass
    codestr = """
        from __static__ import int64, box, clen
        def f(x: list):
            i: int64 = 0
            res = []
            while i < clen(x):
                elem = x[i]
                res.append((box(i), elem))
                i += 1
            return res
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "SEQUENCE_GET", SEQ_LIST_INEXACT)
        res = f(mylist([1, 2, 3]))
        self.assertEqual(res, [(0, 1), (1, 2), (2, 3)])
